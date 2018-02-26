{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs          #-}

module SME.Error
  ( ImportingError(..)
  , FrontendErrors(..)
--  , CompilerErrors(..)
  , TypeCheckErrors(..)
  ) where

import           Control.Exception
import           Data.Loc
import           Data.Maybe            (fromMaybe)

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax (Nameable (..), ToString (..),
                                        Typeness (..))

data FrontendErrors
  = DirAlreadyExists FilePath
  | FileNotFound FilePath
  deriving (Exception)

instance Show FrontendErrors where
  show (DirAlreadyExists fp) =
    "Directory " ++ fp ++ " already exists. Use --force to use it anyway"
  show (FileNotFound fp) = "File not found: " ++ fp

-- data CompilerErrors where
--   InternalCompilerError :: String -> CompilerErrors
--   deriving (Exception)

-- instance Show CompilerErrors where
--   show (InternalCompilerError msg) =
--     "Internal compiler error (probable compiler bug): " ++ msg

data TypeCheckErrors where
  DuplicateName
    :: (ToString a, Located a, ToString b, Located b)
    => a
    -> b
    -> TypeCheckErrors
  UndefinedName :: (Pretty a, Located a) => a -> TypeCheckErrors
  TypeMismatchError :: Typeness -> Typeness -> TypeCheckErrors
  ParamCountMismatch
    :: (Located a, Nameable a, Located b, Nameable b)
    => Int
    -> Int
    -> a
    -> b
    -> TypeCheckErrors
  ExpectedBus :: (ToString a, Located a) => a -> TypeCheckErrors
  ExprInvalidInContext :: (Pretty a, Located a) => a -> TypeCheckErrors
  NamedParameterMismatch :: (ToString a, Located a) => a -> a -> TypeCheckErrors
  BusShapeMismatch :: (Show a, Located b) => a -> a -> b -> TypeCheckErrors
  InstanceParamTypeMismatch :: (Located a) => a -> TypeCheckErrors
  ReferencedAsValue :: (Located a, Nameable a) => a -> TypeCheckErrors
  InternalCompilerError :: String -> TypeCheckErrors
  deriving (Exception)

instance Show TypeCheckErrors where
  show (DuplicateName new prev) =
    "Name " ++
    toString new ++
    " already defined at " ++
    displayLoc (locOf new) ++
    ". Previous definition was " ++ displayLoc (locOf prev)
  show (UndefinedName n) =
    "Name " ++ pprr n ++ " is undefined at " ++ displayLoc (locOf n) ++ "."
  show (TypeMismatchError t1 t2) =
    "Cannot match type " ++
    pprr t2 ++
    " with expected type " ++ pprr t1 ++ ". At " ++ displayLoc (locOf t2)
  show (ParamCountMismatch expected actual inst' ent) =
    "Wrong number of parameters. Entity " ++
    toString (nameOf ent) ++
    " (at " ++
    displayLoc (locOf ent) ++
    ")" ++
    " expected " ++
    pluralize expected "parameter" ++
    " but was instantiated with " ++
    pluralize actual "parameter" ++ " at " ++ displayLoc (locOf inst') ++ "."
  show (ExpectedBus i) =
    "Parameter " ++
    toString i ++
    " does not refer to a bus as expected at " ++ displayLoc (locOf i)
  show (ExprInvalidInContext e) =
    "Expression: " ++
    pprr e ++ " is invalid in context at " ++ displayLoc (locOf e)
  show (NamedParameterMismatch expected actual) =
    "The name of parameter " ++
    toString actual ++
    " in instance declaration at " ++
    displayLoc (locOf actual) ++
    " is actually named " ++ toString expected ++ "."
  show (BusShapeMismatch expected actual inst) =
    "Unable to unify bus shapes in instantiation at " ++
    displayLoc (locOf inst) ++
    " expected: " ++ show expected ++ " but saw " ++ show actual ++ "."
  show (InstanceParamTypeMismatch inst) =
    "Wrong parameter type (bus parameter where a constant is expected or vice-versa) in instantiation at " ++
    displayLoc (locOf inst)
  show (ReferencedAsValue def) =
    "Object " ++
    toString (nameOf def) ++
    " referenced as value " ++ displayLoc (locOf def) ++ "."
  show (InternalCompilerError msg) =
    "Internal compiler error (probable compiler bug): " ++ msg

data ImportingError where
  ImportNotFoundError :: FilePath -> SrcLoc -> ImportingError
  UndefinedIdentifierError
    :: (ToString a, Located a) => [a] -> SrcLoc -> ImportingError
  IdentifierClashError
    :: (ToString a, Located a) => [a] -> SrcLoc -> ImportingError
  ParseError :: String -> ImportingError
  deriving (Exception)

instance Show ImportingError where
  show (ImportNotFoundError f l) =
    "Imported file not found " ++ f ++ "." ++ importedFromMsg l
  show (UndefinedIdentifierError ids l) =
    "Unknown identifier(s) " ++ prettyList ids ++ "." ++ importedFromMsg l
  show (IdentifierClashError ids l) =
    "Specific imports " ++
    prettyList ids ++
    " clashes with names already definde in module." ++ importedFromMsg l
  show (ParseError s) = "Parse error at " ++ s

importedFromMsg :: SrcLoc -> String
importedFromMsg (SrcLoc NoLoc) = ""
importedFromMsg (SrcLoc l)     = " Imported from " ++ displayLoc l

prettyList :: (ToString s) => [s] -> String
prettyList []     = ""
prettyList [x]    = toString x
prettyList [x,y]  = toString x ++ " and " ++ toString y
prettyList (x:xs) = toString x ++ ", " ++ prettyList xs

pluralize :: (Num a, Eq a, Show a) => a -> String -> String
pluralize i s = pluralize' i s Nothing

pluralize' :: (Num a, Eq a, Show a) => a -> String -> Maybe String -> String
pluralize' i s s'
  | i == 1 = show i ++ " " ++ s
  | otherwise = show i ++ " " ++ fromMaybe (s ++ "s") s'
