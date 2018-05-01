{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stage manager module. Manages the compilation process and

module SME.Stages
  ( Stages(..)
  , Config(..)
  , compile
  , mkConfig
  , cmdLineOptParser
  , libOptParser
  ) where

import           Control.Exception     (throw, tryJust)
import           Control.Monad         (when)
import           Data.List             (intercalate, nub)
import           Data.Maybe            (fromJust, isJust)
import qualified Data.Text.IO          as TIO
import           Options.Applicative
import           Text.Show.Pretty      (ppShow)

import           Language.SMEIL.Pretty
import           SME.CodeGen
import           SME.Error
import           SME.ImportResolver
import           SME.Reconstruct
import           SME.Representation
import           SME.Simulate
import           SME.TypeCheck

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
  many
    (strOption
       (long "param" <> short 'p' <>
        help
          "Set a network entity parameter. Repeat this option once per parameter. ARG has the format <entity-name>:<param-name>=<value> (no spaces). Parameters in the code will override parameters set here."))
  where
    stagesPP =
      intercalate ", " (map show [ResolveImport, TypeCheck, CodeGen]) ++
      " or " ++ show Retyped

dumpStage :: Config -> Stages -> Bool
dumpStage c s = s `elem` dumpStages c

compile :: Config -> IO ()
compile conf = do
  (df, nameMap) <- resolveImports (inputFile conf)
  when (dumpStage conf ResolveImport) $ TIO.putStrLn $ pprr df
  res <-
    tryJust
      (\(e :: TypeCheckErrors) -> Just (renderError nameMap e))
      (typeCheck df conf)
  tyEnv <-
    case res of
      Left e  -> throw $ CompilerError e
      Right r -> pure r
  when (dumpStage conf TypeCheck) (putStrLn $ ppShow tyEnv)
  genIn <-
    case runSim conf of
      Just its ->
        simulate its tyEnv >>= \case
          Left e -> do
            putStrLn $ renderError nameMap e
            throw e
          Right r -> return (Void <$ r)
      Nothing -> return tyEnv
  let recon = reconstruct genIn
  when (dumpStage conf Retyped) (TIO.putStrLn $ pprr recon)
  res' <-
    tryJust
      (\(e :: TypeCheckErrors) -> Just (renderError nameMap e))
      (typeCheck recon conf)
  tyEnv' <-
    case res' of
      Left e  -> throw $ CompilerError e
      Right r -> pure r
  when (isJust (outputDir conf)) $
    genOutput (fromJust (outputDir conf)) VHDL tyEnv'
