{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Stage manager module. Manages the compilation process and

module SME.Stages
  ( Stages(..)
  , Config(..)
  , CompilerM (..)
  , compile
  ) where

import           Control.Exception     (throw, tryJust)
import           Control.Monad         (when)
import           Control.Monad.Reader
import           Data.Char             (isLetter, toLower)
import qualified Data.Text.IO          as TIO
import           Text.Show.Pretty      (ppShow)

import           Language.SMEIL.Pretty
import           SME.Error
import           SME.ImportResolver
import           SME.TypeCheck

data Stages
  = ResolveImport
  | Rename
  | TypeCheck
  | Optimize
  | CodeGen
  deriving (Eq, Show)

data Config = Config
  { inputFile        :: FilePath
  , outputDir        :: FilePath
  , dumpStages       :: [Stages] -- ^ Show the output of these stages
  , force            :: Bool -- ^ Overwrite files in preexisting directories
  , strictSizeBounds :: Bool -- ^ Should size bounds in input language be
                        -- strictly enforced
  , inferSizeBounds  :: Bool -- ^ Infer and adjust type bounds during type
                            -- checking
  , warnings         :: Bool -- ^ Enable warnings
  }

dumpStage :: Config -> Stages -> Bool
dumpStage c s = s `elem` dumpStages c

data CompilerState = CompilerState
  { config    :: Config -- ^ The global compilation pipeline configuration
  , renamings :: RenameState
  }

newtype CompilerM a = CompilerM
  { unGlobalM :: Reader CompilerState a
  } deriving (Functor, Applicative, Monad, MonadReader CompilerState)

instance Read Stages where
  readsPrec _ input =
    let (tok, rest) = span (\l -> isLetter l || l == '-') input
    in case mapFormat tok of
         Just f  -> [(f, rest)]
         Nothing -> []
    where
      mapFormat =
        (`lookup` [ ("resolve-imports", ResolveImport)
                  , ("type-check", TypeCheck)
                  , ("optimize", Optimize)
                  , ("code-generation", CodeGen)
                  ]) .
        map toLower

compile :: Config -> IO ()
compile conf = do
  (df, nameMap) <- resolveImports (inputFile conf)
  when (dumpStage conf ResolveImport) $ TIO.putStrLn $ pprr df
  res <-
    tryJust
      (\(e :: TypeCheckErrors) -> Just (renderError nameMap e))
      (typeCheck df)
  tyEnv <- case res of
            Left e  -> throw $ CompilerError e
            Right r -> pure r
  when (dumpStage conf TypeCheck) (putStrLn $ ppShow tyEnv)
  return ()
