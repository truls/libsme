{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Transform the SMEIL AST to a sequence of flat lists which are easier to manipulate.

module SME.Representation
  ( ReprM
  , BaseDefType(..)
  , BaseEnv(..)
  , BaseTopDef(..)
  , BusShape(..)
  , BusState(..)
  , MonadRepr(..)
  , ParamType(..)
  , Void(..)
  , ensureUndef
  , mkEnv
  , runReprM
  , runReprMidentity
  , unReprM
  ) where

import           Control.Exception      (throw)
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (MonadState, StateT, gets, modify,
                                         runStateT)
import qualified Data.HashMap.Strict    as M
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as N
import           Data.Loc               (Located, locOf, noLoc)
import qualified Data.Set               as S

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.Error

--import           Debug.Trace                 (trace)
trace :: String -> a -> a
trace _ = id

newtype ReprM m s a = ReprM
  { unReprM :: ExceptT TypeCheckErrors (StateT (BaseEnv s) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState (BaseEnv s)
             , MonadError TypeCheckErrors
             )

-- TODO: Figure out something nicer instead of these functions
runReprMidentity ::
     BaseEnv s -> ReprM Identity s a -> (Either TypeCheckErrors a, BaseEnv s)
runReprMidentity e f = runIdentity $ runStateT (runExceptT $ unReprM f) e

runReprM :: BaseEnv s -> ReprM m s a -> m (Either TypeCheckErrors a, BaseEnv s)
runReprM e f = runStateT (runExceptT $ unReprM f) e

-- | Type checking monad
class ( Monad m
      , MonadState (BaseEnv s) m
      -- , MonadWriter Log m
      , MonadError TypeCheckErrors m
      ) =>
      MonadRepr s m where

  lookupDef :: (References a) => a -> m (BaseDefType s)
  lookupDef r = do
    (_, res) <- lookupDef' r
    return res

  lookupDef' :: (References a) => a -> m ([Ident], BaseDefType s)
  lookupDef' r = go (trace ("Lookup called with " ++ show (refOf r)) (refOf r))
      -- Three cases.
      -- Case one: Single name [Ident] is found in local scope. Return that
      -- Case two: First name [Ident, ..] refers to a bus in local scope. Return
      -- that.
      -- Case three: Name [Ident, ...] refers to a top-level name. Switch
      -- scope, remaining name compounds should fall into previous cases
    where
      --go [] = throw $ InternalCompilerError "Empty reference."
      go (i :| [])  = do
        d <- gets curBaseEnv
        res <- lookupOrFail i (symTable d)
        return ([], res)
      go (i :| is) = do
        d <- gets curBaseEnv
        lookupOrFail i (symTable d) >>= \case
          b@BusDef {} -> pure (is, b)
          InstDef {instantiated = instantiated} ->
            -- FIXME: N.fromList is a partial function
            withScope instantiated (go (N.fromList is))
          ParamDef {paramType = BusPar {..}} ->
            case ref of
              (r':rs') -> withScope r' (go (N.fromList (rs' ++ is)))
              _ ->
                throw $ InternalCompilerError "Bus reference is a single name"
          _ ->
            error
              ("TODO: Proper error, Name is not of an indexable type " ++ show i) `catchError`
            (\_ -> withScope i (go (N.fromList is)))

  lookupTopDef :: (References a) => a -> m (BaseTopDef s)
  lookupTopDef i =
    go $ trace ("lookupTopDef called with " ++ show (refOf i)) (refOf i)
    where
      go (ident :| []) = do
        ds <- gets defs
        lookupOrFail ident ds >>= \case
          EmptyTable ->
            throw $ InternalCompilerError "Lookup returned empty table"
          e -> pure e
      go _ =
        throw $
        InternalCompilerError
          "Compound name should not be present at this point"

  updateTopDef :: (References a) => a -> (BaseTopDef s -> BaseTopDef s) -> m ()
  updateTopDef i f = do
    def <- lookupTopDef i
    modify (\x -> x {defs = M.insert (toString (nameOf def)) (f def) (defs x)})

  mapDefs :: (References a) => a -> (BaseDefType s -> m (BaseDefType s)) -> m ()
  mapDefs d f = do
    def <- lookupTopDef d
    let tab = symTable def
        ks = M.keys tab
        defs = M.elems tab
    res <- mapM f defs
    updateTopDef d (\x -> x {symTable = M.fromList (zip ks res)})

  forUsedTopDefs :: (BaseTopDef s -> m ()) -> m ()
  forUsedTopDefs f = do
    u <- gets used
    mapM_
      (\x -> do
         def <- lookupTopDef x
         f def)
      u

  addDefinition ::
       (References a, Located b, ToString b) => a -> b -> BaseDefType s -> m ()
  addDefinition topDef k v = do
    def <- lookupTopDef topDef
    symTab <-
      ensureUndef k (symTable def) $
      pure $ M.insert (toString k) v (symTable def)
    updateTopDef topDef (\x -> x {symTable = symTab})

  addUsedEnt :: (Nameable a) => a -> m ()
  addUsedEnt e = modify (\x -> x {used = S.insert (nameOf e) (used x)})

  withScope :: (References a) => a -> m b -> m b
  withScope s act = do
    cur <- gets curBaseEnv
    e <- lookupTopDef s
    modify (\x -> x {curBaseEnv = e})
    act <* modify (\x -> x {curBaseEnv = cur})

  addTopDef :: BaseTopDef s -> m ()
  addTopDef d = do
    ds <- gets defs
    let n = toString $ nameOf d
    ensureUndef (nameOf d) ds $ modify (\s -> s {defs = M.insert n d (defs s)})

-- instance (Monad m) => MonadRepr (ReprM m)

  --withScope :: (ToString a, Located a) => a -> m b -> m b
  --lookupInstance :: String -> m [DefType]
  --lookupInstances :: m [DefType]

lookupOrFail ::
     (ToString a, Pretty a, Located a, MonadRepr s m)
  => a
  -> M.HashMap String b
  -> m b
lookupOrFail e m = case M.lookup (toString e) m of
  Just r  -> pure r
  Nothing -> throw $ UndefinedName e

-- | Tries to look up an element 'i' in map 'm'. Performs action 'a' if
-- successful and throws an error otherwise
ensureUndef ::
     (MonadRepr s m, Nameable d, ToString a, Located a) => a -> M.HashMap String d -> m b -> m b
ensureUndef i m a =
  case M.lookup (toString i) m of
    Just r  -> throw $ DuplicateName i (nameOf r)
    Nothing -> a

data Void = Void
  deriving Show

-- | Type for representing a value in SMEIL
data Value
  = IntVal Type
           Integer
  | ArrayVal Type
             Integer
             [Value]
  | BoolVal Bool
  | DoubleVal Type
              Double
  | SingleVal Type
              Float
  deriving (Show)

-- | Type for representing definitions in SMEIL
data BaseDefType a
  = VarDef { varName :: Ident
           , varDef  :: Variable
           , ext     :: a}
  | ConstDef { constName :: Ident
             , constDef  :: Constant
             , ext       :: a}
  | BusDef { busName  :: Ident
           , busRef   :: [Ident]
           , busShape :: BusShape
           , busDef   :: Bus
           , ext      :: a}
  | FunDef { funcName :: Ident
           , funcDef  :: Function
           , ext      :: a}
  | EnumDef { enumName   :: Ident
            , enumFields :: [(Ident, Integer)]
            , enumDef    :: Enumeration
            , ext        ::a }
  | EnumFieldDef { fieldName  :: Ident
                 , fieldValue :: Integer
                 , defIn      :: Ident
                 , ext        :: a}
  | InstDef { instName     :: Ident
            , instDef      :: Instance
            , instantiated :: Ident
            , params       :: [(String, Value)]
            , anonymous    :: Bool
            , ext          ::a
              -- Also track: Parameters
             }
  | ParamDef { paramName :: Ident
             , paramType :: ParamType
             , ext       ::a }
  deriving (Show)

instance Nameable (BaseDefType a) where
  nameOf VarDef {..}       = varName
  nameOf ConstDef {..}     = constName
  nameOf BusDef {..}       = busName
  nameOf FunDef {..}       = funcName
  nameOf EnumDef {..}      = enumName
  nameOf EnumFieldDef {..} = fieldName
  nameOf InstDef {..}      = instName
  nameOf ParamDef {..}     = paramName

newtype BusShape = BusShape [(Ident, Typeness)]
  deriving (Eq, Show)

data ParamType
  = ConstPar Typeness
  | BusPar { ref      :: [Ident]
           , busShape :: BusShape
           , busState :: BusState
           , array    :: Maybe Integer}
  deriving (Show, Eq)

data BusState
  = Input
  | Output
  | Unused
  deriving (Show, Eq)

data BaseTopDef a
  = ProcessTable { symTable :: M.HashMap String (BaseDefType a)
                 , procName :: Ident
                 , params   :: M.HashMap String ParamType
                 , stms     :: [Statement]
                 , procDef  :: Process
                 , ext      :: a
                 }
  | NetworkTable { symTable :: M.HashMap String (BaseDefType a)
                 , netName  :: Ident
                 , params   :: M.HashMap String ParamType
                 , netDef   :: Network
                 , ext      :: a}
  | EmptyTable
  deriving (Show)

instance Nameable (BaseTopDef a) where
  nameOf ProcessTable {..} = procName
  nameOf NetworkTable {..} = netName
  nameOf EmptyTable        = Ident "" noLoc

instance Located (BaseTopDef a) where
  locOf ProcessTable {..} = locOf procDef
  locOf NetworkTable {..} = locOf netDef
  locOf EmptyTable {}     = noLoc

-- | Top-level type checking environment
data BaseEnv a = BaseEnv
  { defs       :: M.HashMap String (BaseTopDef a) -- ^ Top-level symbol table
  , curBaseEnv :: BaseTopDef a -- ^ Scope of currently used definition
  , used       :: S.Set Ident -- ^ Instantiated definitions
  , ext        :: a
  } deriving (Show)

mkEnv :: a -> BaseEnv a
mkEnv = BaseEnv M.empty EmptyTable S.empty
