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
  , VarState(..)
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
import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
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
      go (i :| []) = do
        d <- gets curBaseEnv
        res <- lookupEx i (symTable d)
        return ([], res)
      go (i :| is) = do
        d <- gets curBaseEnv
        lookupEx i (symTable d) >>= \case
          b@BusDef {} -> pure (is, b)
          InstDef {instantiated = instantiated}
            -- N.fromList is a partial function but is probably safe to use here
            -- as the preceding pattern match of go matches the case where is is
            -- empty
           -> withScope instantiated (go (N.fromList is))
          ParamDef {paramType = BusPar {..}} ->
            case ref of
              (r':rs') -> withScope r' (go (N.fromList (rs' ++ is)))
              _ ->
                throw $ InternalCompilerError "Bus reference is a single name"
          _
            -- If first name component doesn't resolve to a possible compound
            -- name in current scope, it probably refers to a top-level
            -- construct, so we look again in that
           -> withScope i (go (N.fromList is))
  lookupTopDef :: (References a, Located a, Pretty a) => a -> m (BaseTopDef s)
  lookupTopDef i =
    go $ trace ("lookupTopDef called with " ++ show (refOf i)) (refOf i)
    where
      go (ident :| []) = do
        ds <- gets defs
        lookupEx ident ds >>= \case
          EmptyTable ->
            throw $ InternalCompilerError "Lookup returned EmptyTable"
          e -> pure e
      go _
        -- Top-level definitions will only be accessed through compound names if
        -- they reside in imported modules. Any such accesses leading to defined
        -- modules have been renamed by ImportResolver at this point. Therefore,
        -- the use of such a name at this point means that the name refers to a
        -- non-existing entity
       = throw $ UndefinedName i
  updateTopDef ::
       (References a, Located a, Pretty a)
    => a
    -> (BaseTopDef s -> BaseTopDef s)
    -> m ()
  updateTopDef i f = do
    def <- lookupTopDef i
    modify (\x -> x {defs = M.insert (toString (nameOf def)) (f def) (defs x)})
  updateDefsM_ ::
       (References a, Located a, Pretty a)
    => a
    -> (BaseDefType s -> m (BaseDefType s))
    -> m ()
  updateDefsM_ d f = do
    def <- lookupTopDef d
    let tab = symTable def
        ks = M.keys tab
        defs = M.elems tab
    res <- mapM f defs
    updateTopDef d (\x -> x {symTable = M.fromList (zip ks res)})
  mapDefsM ::
       (References a, Located a, Pretty a)
    => a
    -> (BaseDefType s -> m a)
    -> m [a]
  mapDefsM d f = do
    def <- lookupTopDef d
    let tab = symTable def
        defs = M.elems tab
    mapM f defs
  forUsedTopDefsM_ :: (BaseTopDef s -> m ()) -> m ()
  -- FIXME: The "used" here is probably a bug as unused top-level entities
  -- should be filtered away by
  forUsedTopDefsM_ f = void $ mapUsedTopDefsM f
  mapUsedTopDefsM :: (BaseTopDef s -> m a) -> m [a]
  mapUsedTopDefsM f = do
    u <- gets used
    mapM
      (\x -> do
         def <- lookupTopDef x
         f def)
      (S.toList u)
  addDefinition ::
       (References a, Located a, Pretty a, Located b, ToString b)
    => a
    -> b
    -> BaseDefType s
    -> m ()
  addDefinition topDef k v = do
    def <- lookupTopDef topDef
    symTab <-
      ensureUndef k (symTable def) $
      pure $ M.insert (toString k) v (symTable def)
    updateTopDef topDef (\x -> x {symTable = symTab})
  addUsedEnt :: (Nameable a) => a -> m ()
  addUsedEnt e = modify (\x -> x {used = S.insert (nameOf e) (used x)})
  withScope :: (References a, Located a, Pretty a) => a -> m b -> m b
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

lookupEx ::
     (ToString a, Pretty a, Located a, MonadRepr s m)
  => a
  -> M.HashMap String b
  -> m b
lookupEx e m = case M.lookup (toString e) m of
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
  = VarDef { varName  :: Ident
           , varDef   :: Variable
           , varState :: VarState
           , ext      :: a}
  | ConstDef { constName  :: Ident
             , constDef   :: Constant
             , constState :: VarState
             , ext        :: a}
  | BusDef { busName  :: Ident
           , busRef   :: [Ident]
           , busShape :: BusShape
           , busDef   :: Bus
           , busState :: BusState
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
  | Local
  | Unassigned
  deriving (Show, Eq)

data VarState
  = Used
  | Unused
  deriving (Eq, Show)

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
