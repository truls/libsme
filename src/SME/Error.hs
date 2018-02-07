{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module SME.Error
  ( Error(..)
  , FrontendErrors(..)
  , TypeCheckErrors(..)
  ) where

import           Control.Exception
import           Data.Loc

import           Language.SMEIL.Syntax

-- data SomeCompilerException =
--   forall e. Exception e =>
--             SomeCompilerException e

-- instance Show SomeCompilerException where
--   show (SomeCompilerException e) = show e

-- instance Exception SomeCompilerException

-- compilerExceptionToExpcetion :: Exception e => e -> SomException
-- compilerExceptionToException ::

data FrontendErrors
  = DirAlreadyExists FilePath
  | FileNotFound FilePath
  deriving (Exception)

instance Show FrontendErrors where
  show (DirAlreadyExists fp) =
    "Directory " ++ fp ++ " already exists. Use --force to use it anyway"
  show (FileNotFound fp) = "File not found: " ++ fp

data TypeCheckErrors where
  DuplicateName :: (ToString a, Located a) => a -> a -> TypeCheckErrors
  deriving (Exception)

instance Show TypeCheckErrors where
  show (DuplicateName new prev) =
    "Name " ++
    toString new ++
    " already defined at " ++
    displayLoc (locOf new) ++
    ". Previous definition was " ++ displayLoc (locOf prev)

data Error
  = ImportNotFoundError FilePath
                        SrcLoc
  | UndefinedIdentifierError [String]
                             SrcLoc
  | ParseError String
  deriving (Exception)

instance Show Error where
  show (ImportNotFoundError f l) =
    "Imported file not found " ++ f ++ "." ++ importedFromMsg l
  show (UndefinedIdentifierError ids l) =
    "Unknown identifier(s) " ++ show ids ++ "." ++ importedFromMsg l
  show (ParseError s) = "Parse error at " ++ s

importedFromMsg :: SrcLoc -> String
importedFromMsg (SrcLoc NoLoc) = ""
importedFromMsg (SrcLoc l)     = " Imported from " ++ displayLoc l
