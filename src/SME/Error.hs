{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module SME.Error
  ( ImportingError(..)
  , FrontendErrors(..)
  , SomeCompilerException(..)
  , TypeCheckErrors(..)
  , RenderError(..)
  , SimulationError(..)
  , BaseCompilerException (..)
  , NameMap
  , bad

  -- Control.Exception.Safe reexports
  , MonadThrow
  , MonadCatch
  , SomeException
  , throw
  , throwEither
  ) where

import           Control.Exception.Safe (Exception (..), MonadCatch, MonadThrow,
                                         SomeException, throw)
import           Data.Containers        (IsMap (..))
import qualified Data.HashMap.Strict    as M
import           Data.Loc               (Loc (NoLoc), Located, SrcLoc (SrcLoc),
                                         displayLoc)
import           Data.Maybe             (fromMaybe)
import           Data.Typeable          (cast)

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax  (Ident, Nameable (..), Ref,
                                         ToString (..), Typeness (..))
import           SME.Util

import           Text.Show.Pretty       (ppShow)

type NameMap = M.HashMap Ident Ref

--------------------------------------------------------------------------------
-- Top level exception hierarchy

data SomeCompilerException =
  forall e. Exception e =>
            SomeCompilerException e

instance Show SomeCompilerException where
  show (SomeCompilerException e) = show e

instance Exception SomeCompilerException

compilerExceptionToException :: Exception e => e -> SomeException
compilerExceptionToException = toException . SomeCompilerException

compilerExceptionFromException :: Exception e => SomeException -> Maybe e
compilerExceptionFromException x = do
    SomeCompilerException a <- fromException x
    cast a

newtype BaseCompilerException = BaseCompilerException String

instance Show BaseCompilerException where
  show (BaseCompilerException e) = e

instance Exception BaseCompilerException where
  toException = compilerExceptionToException
  fromException = compilerExceptionFromException

--------------------------------------------------------------------------------
-- Static and runtime exceptions

data SomeStaticException = forall e.Exception e => SomeStaticException e

instance Show SomeStaticException where
  show (SomeStaticException e) = show e

instance Exception SomeStaticException where
  toException = compilerExceptionToException
  fromException = compilerExceptionFromException

staticExceptionToException :: Exception e => e -> SomeException
staticExceptionToException = toException . SomeStaticException

staticExceptionFromException :: Exception e => SomeException -> Maybe e
staticExceptionFromException x = do
  SomeStaticException a <- fromException x
  cast a


data SomeRuntimeException = forall e.Exception e => SomeRuntimeException e

instance Show SomeRuntimeException where
  show (SomeRuntimeException e) = show e

instance Exception SomeRuntimeException where
  toException = compilerExceptionToException
  fromException = compilerExceptionFromException

runtimeExceptionToException :: Exception e => e -> SomeException
runtimeExceptionToException = toException . SomeRuntimeException

runtimeExceptionFromException :: Exception e => SomeException -> Maybe e
runtimeExceptionFromException x = do
  SomeRuntimeException a <- fromException x
  cast a

------------------------------------------------------------------------
-- Actual exceptions

data FrontendErrors
  = DirAlreadyExists FilePath
  | FileNotFound FilePath

instance Exception FrontendErrors

instance Show FrontendErrors where
  show (DirAlreadyExists fp) =
    "Directory " ++ fp ++ " already exists. Use --force to use it anyway"
  show (FileNotFound fp) = "File not found: " ++ fp

data SimulationError where
  AssertionError :: (Located a, Pretty a) => a -> Maybe String -> SimulationError

instance Show SimulationError where
  show (AssertionError expr str) =
    let msg = fromMaybe (pprrString expr) str
    in concat ["Assertion ", msg, " failed on line ", displayLoc' expr]

instance Exception SimulationError where
  toException = runtimeExceptionToException
  fromException = runtimeExceptionFromException

data UnexpectedError where
  InternalCompilerError :: String -> UnexpectedError

instance Exception UnexpectedError where
  toException = staticExceptionToException
  fromException = staticExceptionFromException

instance Show UnexpectedError where
  show (InternalCompilerError msg) =
    "Internal compiler error (probable compiler bug): " ++ msg


class (IsMap map, Show ex) => RenderError map ex where
  renderError :: map -> ex -> String

bad :: (MonadThrow m) => String -> m a
bad = throw . InternalCompilerError

throwEither :: (MonadThrow m, Exception e) => Either e a -> m a
throwEither (Right r) = return r
throwEither (Left l)  = throw l

instance RenderError NameMap TypeCheckErrors where
  renderError m (ParamCountMismatch expected actual inst' ent) =
    concat
      [ "Wrong number of parameters. Entity "
      , origName ent m
      , " (at "
      , displayLoc' ent
      , ")"
      , " expected "
      , pluralize expected "parameter"
      , " but was instantiated with "
      , pluralize actual "parameter"
      , " at "
      , displayLoc' inst'
      , "."
      ]
  renderError m (NamedParameterMismatch expected actual ent) =
    concat
      [ "The name of parameter "
      , toString actual
      , " in instance declaration of "
      , origName ent m
      , " at "
      , displayLoc' actual
      , " is actually named "
      , toString expected
      , "."
      ]
  renderError _ e = show e

origName :: (Nameable a) => a -> NameMap -> String
origName k m = case M.lookup (nameOf k) m of
  Just r  -> pprrString r
  Nothing -> toString (nameOf k)

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
  NamedParameterMismatch
    :: (ToString a, Located a, Nameable b) => a -> a -> b -> TypeCheckErrors
  BusShapeMismatch :: (Show a, Located b) => a -> a -> b -> TypeCheckErrors
  InstanceParamTypeMismatch :: (Located a) => a -> TypeCheckErrors
  ReferencedAsValue :: (Located a, Nameable a) => a -> TypeCheckErrors
  BusReferencedAsValue :: (Located a, Pretty a) => a -> TypeCheckErrors
  WroteConstant :: (Located a, Pretty a) => a -> TypeCheckErrors
  WroteInputBus :: (Located a, Pretty a) => a -> TypeCheckErrors
  ReadOutputBus :: (Located a, Pretty a) => a -> TypeCheckErrors
  NotAnArray :: (Located a, Pretty a) => a -> TypeCheckErrors
  ArgumentError :: (Located a) => a -> String -> TypeCheckErrors
  FormatStringMismatch :: (Show a, Located b) => a -> a -> b -> TypeCheckErrors
  BreakOutsideLoop :: (Located a) => a -> TypeCheckErrors
  ArrayIndexOutOfBounds :: (Located a) => a -> TypeCheckErrors
  OperandType :: (Located a, Pretty a, Pretty b) => a -> b -> TypeCheckErrors

instance Exception TypeCheckErrors where
  toException = staticExceptionToException
  fromException = staticExceptionFromException


instance Show TypeCheckErrors where
  show e@NamedParameterMismatch {} = renderError (M.empty :: NameMap) e
  show e@ParamCountMismatch {} = renderError (M.empty :: NameMap) e
  show (DuplicateName new prev) =
    concat
      [ "Name "
      , toString new
      , " already defined at "
      , displayLoc' new
      , ". Previous definition was "
      , displayLoc' prev
      ]
  show (UndefinedName n) =
    concat ["Name ", pprrString n, " is undefined at ", displayLoc' n, "."]
  show (TypeMismatchError t1 t2) =
    concat
      [ "Cannot match type "
      , pprrString t2
      , " with expected type "
      , pprrString t1
      , ". At "
      , displayLoc' t2
      ]
  show (ExpectedBus i) =
    concat
      [ "Parameter "
      , toString i
      , " does not refer to a bus as expected at "
      , displayLoc' i
      ]
  show (ExprInvalidInContext e) =
    concat
      [ "Expression: "
      , pprrString e
      , " is invalid in context at "
      , displayLoc' e
      ]
  show (BusShapeMismatch expected actual inst) =
    concat
      [ "Unable to unify bus shapes in instantiation at "
      , displayLoc' inst
      , " expected: \n"
      , ppShow expected
      , "\n\nbut saw\n\n"
      , ppShow actual
      , "."
      ]
  show (InstanceParamTypeMismatch inst) =
    concat
      [ "Wrong parameter type (bus parameter where a constant is"
      , " expected or vice-versa) in instantiation at "
      , displayLoc' inst
      ]
  show (ReferencedAsValue def) =
    concat
      [ "Object "
      , toString (nameOf def)
      , " referenced as value "
      , displayLoc' def
      , "."
      ]
  show (BusReferencedAsValue def) =
    concat
      [ "Bus "
      , pprrString def
      , " referenced as value at "
      , displayLoc' def
      , ". Maybe you meant to access one of its channels?"
      ]
  show (WroteInputBus def) =
    concat
      ["Cannot write to input bus ", pprrString def, " at ", displayLoc' def]
  show (WroteConstant def) =
    concat
      [ "Cannot write to read-only constant "
      , pprrString def
      , " at "
      , displayLoc' def
      ]
  show (ReadOutputBus def) =
    concat
      ["Cannot read from output bus ", pprrString def, " at ", displayLoc' def]
  show (NotAnArray def) =
    concat [pprrString def, " is not an array at ", displayLoc' def]
  show (ArgumentError def msg) = concat [msg, " At: ", displayLoc' def]
  show (FormatStringMismatch expected actual def) =
    concat
      [ "Format string expected "
      , show expected
      , " parameters, but was only given "
      , show actual
      , ". At "
      , displayLoc' def
      ]
  show (BreakOutsideLoop def) =
    "Break statement outside loop at " ++ displayLoc' def
  show (ArrayIndexOutOfBounds def) = "Index out of bounds " ++ displayLoc' def
  show (OperandType op ty) =
    concat
      [ "Unsupported type "
      , pprrString ty
      , " for operand "
      , pprrString op
      , " At "
      , displayLoc' op
      ]

data ImportingError where
  ImportNotFoundError :: FilePath -> SrcLoc -> ImportingError
  UndefinedIdentifierError
    :: (ToString a, Located a) => [a] -> SrcLoc -> ImportingError
  IdentifierClashError
    :: (ToString a, Located a) => [a] -> SrcLoc -> ImportingError
  ParseError :: String -> ImportingError

instance Exception ImportingError where
  toException = staticExceptionToException
  fromException = staticExceptionFromException

instance Show ImportingError where
  show (ImportNotFoundError f l) =
    concat ["Imported file not found ", f, ".", importedFromMsg l]
  show (UndefinedIdentifierError ids l) =
    concat ["Unknown identifier(s) ", prettyList ids, ".", importedFromMsg l]
  show (IdentifierClashError ids l) =
    concat
      [ "Specific imports "
      , prettyList ids
      , " clashes with names already defined in module."
      , importedFromMsg l
      ]
  show (ParseError s) = "Parse error at " ++ s

importedFromMsg :: SrcLoc -> String
importedFromMsg (SrcLoc NoLoc) = ""
importedFromMsg (SrcLoc l)     = " Imported from " ++ displayLoc l

prettyList :: (ToString s, Located s) => [s] -> String
prettyList []     = ""
prettyList [x]    = toString x ++ "(defined at " ++ displayLoc' x ++ ")"
prettyList [x, y] = prettyList [x] ++ " and " ++ prettyList [y]
prettyList (x:xs) = prettyList [x] ++ ", " ++ prettyList xs

pluralize :: (Num a, Eq a, Show a) => a -> String -> String
pluralize i s = pluralize' i s Nothing

pluralize' :: (Num a, Eq a, Show a) => a -> String -> Maybe String -> String
pluralize' i s s'
  | i == 1 = show i ++ " " ++ s
  | otherwise = show i ++ " " ++ fromMaybe (s ++ "s") s'
