{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Stage manager module. Manages the compilation process and

module SME.Stages
  ( Stages(..)
  , Config(..)
  , CompilerM
  , CompilerState(..)
  , mkCompilerState
  , runCompilerM
  , compile
  , mkConfig
  , cmdLineOptParser
  , libOptParser
  , printWarnings
  , exitOnError
  , doImports
  , doTypeCheck
  , doReconstruct
  , doOutput
  , handleErrors
  ) where

import           Control.Exception.Safe (Handler (..), IOException,
                                         SomeException (..), catches, try)
import           Control.Monad          (mapM_)
import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashMap.Strict    as M
import           Data.List              (intercalate, nub)
import qualified Data.Text.IO           as TIO
import           Options.Applicative
import           System.Exit            (exitFailure)
import           System.IO              (hPrint, stderr)

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax  (DesignFile)
import           SME.CodeGen
import           SME.Error
import           SME.ImportResolver
import           SME.Reconstruct
import           SME.Representation
import           SME.Simulate
import           SME.TypeCheck
import           SME.Warning

cmdLineOptParser :: Parser Config
cmdLineOptParser =
  optParser $
  strOption
    (long "input" <> metavar "IN" <> short 'i' <>
     help "Input file. Specify - for stdin.")

libOptParser :: Parser Config
libOptParser =
  optParser $
  strOption
    (long "input" <> metavar "IN" <> short 'i' <>
     help "Input file. Specify - for stdin." <>
     value "")

optParser :: Parser String -> Parser Config
optParser p =
  Config <$> p <*>
  optional
    (strOption
       (long "output" <> metavar "OUT" <> short 'o' <>
        help
          "If VHDL code generation is desired, set this option to the directory where output is wanted. Defaults to no-output")) <*>
  (nub <$>
   many
     (option
        auto
        (long "dump-stage" <>
         help
           ("For debugging. Dumps the state of the compiler resulting from stage. Can be specified multiple times with one of the following arguments " ++
            stagesPP)))) <*>
  switch
    (long "force" <> short 'f' <>
     help "Allow existing output directories and overwrite files") <*>
  switch
    (long "no-strict-size-bounds" <>
     help "Interpret all sizes as if they were dynamically sized") <*>
  -- switch
  --   (long "adjust-size-bounds" <>
  --    help "Adjust size bounds in code to inferred values") <*>
  switch (long "quiet" <> short 'q' <> help "Suppress output from simulated programs") <*>
  switch (long "quiet" <> short 'q' <> help "Suppress output from simulated programs") <*>
  switch
    (long "emulate-overflows" <>
     help
       "Strictly enforce the bit width of types during simulation by truncating overflowing numbers to their least significant bits. Default behavior is to simply warn about the overflow.") <*>
  optional
    (option
       auto
       (long "simulate" <> short 's' <>
        help "Run simulation for the given number of iterations")) <*>
  optional
    (strOption
       (long "trace" <> short 't' <> help "Where to write the trace file")) <*>
  switch (long "no-warnings" <> short 'w' <> help "Disable warnings") <*>
  switch (long "no-range-annot" <> help "Disable warnings") <*>
  many
    (strOption
       (long "param" <> short 'p' <>
        help
          "Set a network entity parameter. Repeat this option once per parameter. ARG has the format <entity-name>:<param-name>=<value> (no spaces). Parameters in the code will override parameters set here."))
  where
    stagesPP =
      intercalate ", " (map show [ResolveImport, TypeCheck, CodeGen]) ++
      " or " ++ show Retyped

newtype CompilerM a = CompilerM
  { unCompilerM :: ReaderT Config (StateT CompilerState IO) a
  } deriving ( Monad
             , Applicative
             , Functor
             , MonadState CompilerState
             , MonadReader Config
             , MonadIO
             , MonadThrow
             , MonadCatch
             )

runCompilerM :: Config -> CompilerState -> CompilerM a -> IO (a, CompilerState)
runCompilerM c s f = runStateT (runReaderT (unCompilerM f) c) s

data CompilerState = CompilerState
  { nameMap  :: NameMap
  , warnings :: Maybe Warns
  }

mkCompilerState :: CompilerState
mkCompilerState = CompilerState {nameMap = M.empty, warnings = Nothing}

doImports :: FilePath -> CompilerM DesignFile
doImports fp = do
  (df, nameMap') <- resolveImports fp
  modify (\x -> x { nameMap = nameMap' })
  return df

doTypeCheck :: DesignFile -> CompilerM (BaseEnv Void)
doTypeCheck df = do
  (tyEnv, warns) <- typeCheck df =<< ask
  modify (\x -> x { warnings = Just warns })
  return tyEnv

-- fromSimEnv :: SimEnv -> CompilerM (BaseEnv Void)
-- fromSimEnv = pure . (<$) Void

doSimulate :: BaseEnv Void -> CompilerM (BaseEnv Void)
doSimulate e =
  asks runSim >>= \case
      Just its ->
        liftIO $ (Void <$) <$> simulate its e
      Nothing -> return e

doReconstruct :: BaseEnv Void -> CompilerM DesignFile
doReconstruct = pure . reconstruct

doOutput :: BaseEnv Void -> CompilerM ()
doOutput e = asks outputDir >>= \case
  Just d ->
    liftIO $ genOutput d VHDL e
  Nothing -> return ()

dumpStage :: (Pretty a) => Stages -> a -> CompilerM a
dumpStage st v = do
  whenM ((st `elem`) <$> asks dumpStages) (liftIO $ TIO.putStrLn $ pprr v)
  return v

pipeline :: FilePath -> CompilerM ()
pipeline =
  doImports >=>
  dumpStage ResolveImport >=>
  doTypeCheck >=>
  dumpStage TypeCheck >=>
  doSimulate >=>
  doReconstruct >=>
  dumpStage Retyped >=>
  doTypeCheck >=>
  doOutput

doCompile :: FilePath -> CompilerM ()
doCompile fp = handleErrors $ pipeline fp >> printWarnings

printWarnings :: CompilerM ()
printWarnings =
  unlessM (asks noWarnings) $ mapM_ (liftIO . print) =<< gets warnings

handleErrors :: CompilerM a -> CompilerM a
handleErrors act = do
  nm <- gets nameMap
  let hs =
        [ Handler
            (\(e :: TypeCheckErrors) ->
               throw $ BaseCompilerException $ renderError nm e)
        , Handler
            (\(e :: SomeCompilerException) ->
               throw $ BaseCompilerException $ show e)
        , Handler
            (\(e :: IOException) ->
               throw $
               BaseCompilerException ("Uncaught IO Exception: " ++ show e))
        ]
  catches act hs

exitOnError :: IO a -> IO a
exitOnError act =
  try act >>= \case
    Left (e :: SomeException) -> do
      hPrint stderr  e
      exitFailure
    Right r -> return r

compile :: Config -> IO ()
compile c = fst <$> runCompilerM c mkCompilerState (doCompile (inputFile c))
