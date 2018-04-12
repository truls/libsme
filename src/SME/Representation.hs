{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Internal representation of an SMEIL program and functions for manipulating
-- it.

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
  , Extension(..)
  , BusVisibility(..)
  , Value (..)
  , BaseSymTab
  , ParamList
  , ensureUndef
  , mkEnv
  , runReprM
  , runReprStM
  , runReprMidentity
  , unReprM
  , isBus
  , mkVarDef
  , lookupEx
  ) where

import           Control.Exception      (throw)
import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.IO.Class
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

--import           Debug.Trace            (trace, traceM)
trace :: String -> a -> a
trace _ = id

traceM :: (Applicative f) => String -> f ()
traceM _ = pure ()

newtype ReprM m s a = ReprM
  { unReprM :: ExceptT TypeCheckErrors (StateT (BaseEnv s) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState (BaseEnv s)
             , MonadError TypeCheckErrors
             , MonadIO
             )

-- TODO: Figure out something nicer instead of these functions
runReprMidentity ::
     BaseEnv s -> ReprM Identity s a -> (Either TypeCheckErrors a, BaseEnv s)
runReprMidentity e f = runIdentity $ runStateT (runExceptT $ unReprM f) e

runReprM :: BaseEnv s -> ReprM m s a -> m (Either TypeCheckErrors a, BaseEnv s)
runReprM e f = runStateT (runExceptT $ unReprM f) e

runReprStM ::
     BaseEnv s -> ReprM (m st) s a -> m st (Either TypeCheckErrors a, BaseEnv s)
runReprStM e f = runStateT (runExceptT $ unReprM f) e

-- | Type checking monad
class ( Monad m
      , MonadState (BaseEnv s) m
      -- , MonadWriter Log m
      , MonadError TypeCheckErrors m
      , Extension s
      ) =>
      MonadRepr s m
  -- | Looks up a definition,
                              where
  lookupDef :: (References a) => a -> m (BaseDefType s)
  lookupDef r = do
    (_, res) <- lookupDef' r
    return res
  {-# INLINEABLE lookupDef #-}
  -- | Looks up a reference returning in a tuple the last part of the reference
  -- (if any) that was not use din looking up the definition, and the definition
  -- that was found.
  lookupDef' :: (References a) => a -> m ([Ident], BaseDefType s)
  lookupDef' r = do
    ti <- curEnvIdent
    go
      (trace
         ("Lookup called with " ++ show (refOf r) ++ " context " ++ show ti)
         (refOf r))
    -- Three cases.
    -- Case one: Single name [Ident] is found in local scope. Return that
    -- Case two: First name [Ident, ..] refers to a bus in local scope. Return
    -- that.
    -- Case three: Name [Ident, ...] refers to a top-level name. Switch
    -- scope, remaining name compounds should fall into previous cases
    where
      go (i :| []) = do
        d <- getCurEnv
        traceM ("Symbol table " ++ show (nameOf d))
        res <- lookupEx i (symTable d)
        return ([], res)
      go (i :| is) = do
        d <- getCurEnv
        case M.lookup (toString i) (symTable d) of
          Just lookupRes ->
            case lookupRes of
              b@BusDef {} -> pure (is, b)
              InstDef {instantiated = instantiated}
              -- N.fromList is a partial function but is probably safe to use here
              -- as the preceding pattern match of go matches the case where is is
              -- empty
               ->
                trace
                  "Got back InstDef "
                  withScope
                  instantiated
                  (go (N.fromList is))
              ParamDef {paramType = BusPar {..}} ->
                trace "GOt back ParamDef" $
                case ref of
                  (_ :| []) ->
                    throw $
                    InternalCompilerError "Bus reference is a single name"
                  (r' :| rs') -> withScope r' (go (N.fromList (rs' ++ is)))
              _
              -- If first name component doesn't resolve to a possible compound
              -- name in current scope, it probably refers to a top-level
              -- construct, so we look again in that
               -> trace "lookup recursing" $ withScope i (go (N.fromList is))
          Nothing ->
            trace "lookup recursing2" $ withScope i (go (N.fromList is))
  {-# INLINEABLE lookupDef' #-}
  -- | Looks up a top-level definition
  lookupTopDef :: (References a, Located a, Pretty a) => a -> m (BaseTopDef s)
  lookupTopDef i =
    go $ trace ("lookupTopDef called with " ++ show (refOf i)) (refOf i)
    where
      go (ident :| []) = do
        ds <- gets defs
        lookupEx ident ds
      go _
        -- Top-level definitions will only be accessed through compound names if
        -- they reside in imported modules. Any such accesses leading to defined
        -- modules have been renamed by ImportResolver at this point. Therefore,
        -- the use of such a name at this point means that the name refers to a
        -- non-existing entity
       = trace "lookupTopDef" $ throw $ UndefinedName i
  --{-# INLINEABLE lookupTopdef #-}
  -- | Updates a top-level definition 'i', by applying a function 'f' to its
  -- top-level definition.
  updateTopDef ::
       (References a, Located a, Pretty a)
    => a
    -> (BaseTopDef s -> BaseTopDef s)
    -> m ()
  updateTopDef i f = do
    def <- lookupTopDef i
    modify (\x -> x {defs = M.insert (toString (nameOf def)) (f def) (defs x)})
  {-# INLINE updateTopDef #-}
  -- | Updates all definitions within the top-level definition 'd', by applying
  -- the function 'f'
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
  curEnvIdent :: m Ident
  curEnvIdent = gets curEnv
  mapDefsM d f = do
    def <- lookupTopDef d
    let tab = symTable def
        defs = M.elems tab
    mapM f defs
  mapUsedTopDefsM_ :: (BaseTopDef s -> m ()) -> m ()
  -- FIXME: The "used" here is probably a bug as unused top-level entities
  -- should be filtered away by
  mapUsedTopDefsM_ f = void $ mapUsedTopDefsM f
  mapUsedTopDefsM :: (BaseTopDef s -> m a) -> m [a]
  mapUsedTopDefsM f = do
    u <- gets used
    mapM
      (\x -> do
         def <- lookupTopDef x
         --withScope x $ f def
         f def)
      (S.toList u)
  addDefinition ::
       (References a, Located a, Pretty a, ToString a)
    => a
    -> BaseDefType s
    -> m ()
  addDefinition k v = do
    cur <- gets curEnv
    addDefinition' cur k v
  addDefinition' ::
       (References a, Located a, Pretty a, Located b, ToString b)
    => a
    -> b
    -> BaseDefType s
    -> m ()
  addDefinition' topDef k v = do
    def <- lookupTopDef topDef
    symTab <-
      ensureUndef k (symTable def) $
      pure $ M.insert (toString k) v (symTable def)
    updateTopDef topDef (\x -> x {symTable = symTab})
  -- | Executes action act with a fresh environment
  withLocalEnv :: m a -> m a
  withLocalEnv act = do
    cur <- getCurEnv
    let symTab = symTable cur
    res <- act
    updateCurEnv (\x -> x {symTable = symTab})
    return res
  addUsedEnt :: (Nameable a) => a -> m ()
  addUsedEnt e = modify (\x -> x {used = S.insert (nameOf e) (used x)})
  --setUsedBus :: Ref -> (Maybe Ident, BusState) -> m ()
  setUsedBus :: Ref -> (Ref, BusState) -> m ()
  setUsedBus r s = do
    cur <- gets curEnv
    traceM ("setUsedbus " ++ show cur ++ " " ++ show r ++ " " ++ show s)
    --withScope (N.head r)
    updateCurEnv
      (\case
         p@ProcessTable {usedBuses = usedBuses} ->
           p {usedBuses = M.insertWith S.union r (S.singleton s) usedBuses}
         other -> other)
  getBusState :: Ref -> m (Maybe [(Ref, BusState)])
  getBusState r =
    M.lookup r . usedBuses <$> getCurEnv >>= \case
      Just s -> pure $ Just $ S.toList s
      Nothing -> pure Nothing --Unassigned
  -- addUsedBus :: (References a) => a -> m ()
  -- addUsedBus r = do
  --   cur <- gets curBaseEnv
  --   modify (\x -> x {1
  withScope :: (References a, Pretty a, Located a) => a -> m b -> m b
  withScope s act = do
    cur <- gets curEnv
    new <- lookupTopDef s
    modify (\x -> x {curEnv = nameOf new})
    act <* modify (\x -> x {curEnv = cur})
  getCurEnv :: (MonadRepr a m) => m (BaseTopDef a)
  getCurEnv = do
    e <- gets curEnv
    traceM ("getCurEnv " ++ show e)
    lookupTopDef e
  updateCurEnv :: (MonadRepr a m) => (BaseTopDef a -> BaseTopDef a) -> m ()
  updateCurEnv f = do
    e <- gets curEnv
    updateTopDef e f
  addTopDef :: BaseTopDef s -> m ()
  addTopDef d = do
    ds <- gets defs
    let n = toString $ nameOf d
    ensureUndef (nameOf d) ds $ modify (\s -> s {defs = M.insert n d (defs s)})
  updateDef ::
       (MonadRepr s m, Located a, References a, Pretty a)
    => a
    -> (BaseDefType s -> BaseDefType s)
    -> m ()
  updateDef d f = do
    cur <- gets curEnv
    def <- lookupDef d
    traceM ("In updateDef: " ++ show (nameOf def)) -- ++ " " ++ show cur)
    let def' = f def
    updateTopDef
      cur
      (\x -> x {symTable = M.insert (toString (nameOf def)) def' (symTable x)})

lookupEx ::
     (ToString a, Pretty a, Located a, MonadRepr s m)
  => a
  -> M.HashMap String b
  -> m b
lookupEx e m = case M.lookup (toString e) m of
  Just r  -> pure r
  Nothing -> trace "lookupEx" $ throw $ UndefinedName e

-- | Tries to look up an element 'i' in map 'm'. Performs action 'a' if
-- successful and throws an error otherwise
ensureUndef ::
     (MonadRepr s m, Nameable d, ToString a, Located a) => a -> M.HashMap String d -> m b -> m b
ensureUndef i m a =
  case M.lookup (toString i) m of
    Just r  -> throw $ DuplicateName i (nameOf r)
    Nothing -> a

class Extension a where
  {-# MINIMAL emptyExt #-}
  emptyExt :: a
  envExt :: BaseEnv a -> a
  envExt = Prelude.const emptyExt
  topExt :: BaseTopDef a -> a
  topExt = Prelude.const emptyExt
  defExt :: BaseDefType a -> a
  defExt = Prelude.const emptyExt

instance Extension Void where
  emptyExt = Void

data Void = Void
  deriving Show

-- data Value
--   = IntVal Type
--            Integer
--   | ArrayVal Type
--              Integer
--              [Value]
--   | BoolVal Bool
--   | DoubleVal Type
--               Double
--   | SingleVal Type
--               Float
--   deriving (Show)

data Value
  = IntVal Integer
  | ArrayVal Int
             [Value]
  | BoolVal Bool
  | DoubleVal Double
  | SingleVal Float
  deriving (Show, Eq)



instance Ord Value
  -- This instance is "bad" as it make an effort to implement a notion of
  -- type-unsafe and somewhat arbitrary comparisons between SMEIL
  -- values. However, should comparisons between values of different types ever
  -- happen, it should be considered a type-checker bug.
                                                         where
  (IntVal a) `compare` (IntVal b) = a `compare` b
  (IntVal a) `compare` (BoolVal False) = a `compare` 0
  (IntVal a) `compare` (BoolVal True) = a `compare` 1
  (IntVal a) `compare` (SingleVal b) = fromIntegral a `compare` b
  (IntVal a) `compare` (DoubleVal b) = fromIntegral a `compare` b
  a@(BoolVal True) `compare` b =
    case b `compare` a of
      LT -> GT
      GT -> LT
      EQ -> EQ
  a@(BoolVal False) `compare` b =
    case b `compare` a of
      LT -> GT
      GT -> LT
      EQ -> EQ
  (DoubleVal a) `compare` (DoubleVal b) = a `compare` b
  (DoubleVal a) `compare` (IntVal b) = a `compare` fromIntegral b
  (DoubleVal a) `compare` (BoolVal False) = a `compare` 1
  (DoubleVal a) `compare` (BoolVal True) = a `compare` 0
  DoubleVal {} `compare` SingleVal {} = GT -- TODO
  (SingleVal a) `compare` (SingleVal b) = a `compare` b
  (SingleVal a) `compare` (IntVal b) = a `compare` fromIntegral b
  (SingleVal a) `compare` (BoolVal False) = a `compare` 1
  (SingleVal a) `compare` (BoolVal True) = a `compare` 0
  (SingleVal _) `compare` (DoubleVal _) = LT -- TODO
  (ArrayVal _ a) `compare` b = maximum a `compare` b
  a `compare` (ArrayVal _ b) = a `compare` maximum b

mkVarDef :: Ident -> Typeness -> a -> BaseDefType a
mkVarDef i t el =
  VarDef
  { varName = i
  , varDef =
      Variable {name = i, ty = t, val = Nothing, range = Nothing, loc = noLoc}
  , varState = Used
  , varVal = LitInt 0 noLoc
  , ext = el
  }

-- | Type for representing definitions in SMEIL
data BaseDefType a
  = VarDef { varName  :: Ident
           , varDef   :: Variable
           , varState :: VarState
           --, varVa
           , varVal   :: Literal
           -- , varRange :: (Integer, Integer)
           , ext      :: a }
  | ConstDef { constName  :: Ident
             , constDef   :: Constant
             , constState :: VarState
             , constVal   :: Literal
             , ext        :: a }
  | BusDef { busName   :: Ident
           , busRef    :: Ref
           , busShape  :: BusShape
           , busDef    :: Bus
           , busState  :: BusState
           , shared    :: BusVisibility -- ^ Property indicating if a bus is used for
                         -- communicating outside of an instance. Starts
                         -- out as true. Flips to false if a bus is read
                         -- from and written to in the same
                         -- cycle. Accessing a bus with shared = false from
                         -- outside the process where it is declared is an error
           , isExposed :: Bool
           , ext       :: a }
  | FunDef { funcName :: Ident
           , funcDef  :: Function
           , ext      :: a }
  | EnumDef { enumName   :: Ident
            , enumFields :: [(Ident, Integer)]
            , isRegular  :: Bool
            , enumDef    :: Enumeration
            , ext        :: a }
  | EnumFieldDef { fieldName  :: Ident
                 , fieldValue :: Integer
                 , defIn      :: Ident
                 , ext        :: a }
  | InstDef { instName     :: Ident
            , instDef      :: Instance
            , instantiated :: Ident
            --, params       :: [Ref]
            , anonymous    :: Bool
            , ext          :: a
              -- Also track: Parameters
             }
  | ParamDef { paramName :: Ident
             , paramType :: ParamType
             , ext       :: a }
  deriving (Show)

isBus :: BaseDefType a -> Bool
isBus BusDef {} = True
isBus _         = False

instance Functor BaseDefType where
  fmap f d@VarDef {..}       = d { ext = f ext}
  fmap f d@ConstDef {..}     = d { ext = f ext}
  fmap f d@BusDef {..}       = d { ext = f ext}
  fmap f d@FunDef {..}       = d { ext = f ext}
  fmap f d@EnumDef {..}      = d { ext = f ext}
  fmap f d@EnumFieldDef {..} = d { ext = f ext}
  fmap f d@InstDef {..}      = d { ext = f ext}
  fmap f d@ParamDef {..}     = d { ext = f ext}

instance Nameable (BaseDefType a) where
  nameOf VarDef {..}       = varName
  nameOf ConstDef {..}     = constName
  nameOf BusDef {..}       = busName
  nameOf FunDef {..}       = funcName
  nameOf EnumDef {..}      = enumName
  nameOf EnumFieldDef {..} = fieldName
  nameOf InstDef {..}      = instName
  nameOf ParamDef {..}     = paramName

newtype BusShape = BusShape
  { unBusShape :: [(Ident, (Typeness, Maybe Literal))]
  } deriving (Eq, Show)

--type BusShape = M.HashMap Ident (Typeness, Maybe Expr)

data InstParam =
  InstConstPar Ref
  | InstBusPar Ref
  deriving (Show, Eq)

data ParamType
  = ConstPar Typeness
  | BusPar { ref      :: Ref
           , localRef :: Ref
           , busShape :: BusShape
           , busState :: BusState
           , array    :: Maybe Integer}
  deriving (Show, Eq)

data BusVisibility
  = Shared
  | Private
  deriving (Show, Eq)

data BusState
  = Input
  | Output
  | Local
  | Unassigned
  deriving (Show, Eq, Ord)

data VarState
  = Used
  | Unused
  deriving (Eq, Show)

type BaseSymTab a = M.HashMap String (BaseDefType a)
type ParamList = [(Ident, ParamType)]

data BaseTopDef a
  = ProcessTable { symTable  :: BaseSymTab a
                 , procName  :: Ident
                 -- FIXME: Consider removing params since the info already
                 -- exists in the symtable
                 , params    :: ParamList
                 --, params    :: M.HashMap Ident ParamType
                 , stms      :: [Statement]
                   --, usedBuses :: M.HashMap Ref (S.Set (Maybe Ident, BusState))
                 , usedBuses :: M.HashMap Ref (S.Set (Ref, BusState))
                 --, referencedProcs ::
                 , procDef   :: Process
                 , ext       :: a }
  | NetworkTable { symTable :: BaseSymTab a
                 , netName  :: Ident
                 , params   :: ParamList
                 , netDef   :: Network
                 , ext      :: a }
  deriving (Show)

instance Functor BaseTopDef where
  fmap f t@ProcessTable {..} =
    t {symTable = (f <$> ) <$>  symTable, ext = f ext}
  fmap f t@NetworkTable {..} =
    t {symTable = (f <$>) <$> symTable, ext = f ext}

instance Nameable (BaseTopDef a) where
  nameOf ProcessTable {..} = procName
  nameOf NetworkTable {..} = netName

instance Located (BaseTopDef a) where
  locOf ProcessTable {..} = locOf procDef
  locOf NetworkTable {..} = locOf netDef

instance Functor BaseEnv where
  fmap f BaseEnv {..} =
    BaseEnv ((f <$>) <$> defs) curEnv used (f ext)

-- | Top-level type checking environment
data BaseEnv a = BaseEnv
  { defs   :: M.HashMap String (BaseTopDef a) -- ^ Top-level symbol table
  --, curEnv :: BaseTopDef a -- ^ Scope of currently used definition
  , curEnv :: Ident -- TODO: Probably change to Maybe Ref
  , used   :: S.Set Ident -- ^ Instantiated definitions
  , ext    :: a
  } deriving (Show)

mkEnv :: a -> BaseEnv a
mkEnv = BaseEnv M.empty (Ident "__noEnv__" noLoc) S.empty
