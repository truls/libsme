{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Internal representation of an SMEIL program with related core datas
-- tructures and functions for manipulating them.

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
  , Value(..)
  , BaseSymTab
  , ParamList
  , UsedBuses
  , ensureUndef
  , mkEnv
  , runReprM
  , runReprMidentity
  , unReprM
  , isBus
  , mkVarDef
  , lookupEx
  , absValue
  , setType
  , setBusChanType
  , lookupTy
  , Config(..)
  , Stages(..)
  , mkConfig
  , InstParam(..)
  , valueToLit
  ) where

import           Control.Exception               (throw)
import           Control.Monad                   (void, when)
import           Control.Monad.Except            (ExceptT, MonadError,
                                                  runExceptT)
import           Control.Monad.Identity          (Identity, runIdentity)
import           Control.Monad.IO.Class
import           Control.Monad.State             (MonadState, StateT, gets,
                                                  modify, runStateT)
import           Data.Char                       (isLetter, toLower)
import           Data.Data                       (Data)
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import qualified Data.List.NonEmpty              as N
import           Data.Maybe                      (fromJust, isJust)

import qualified Data.HashMap.Strict             as M
import           Data.Loc                        (Located, locOf, noLoc)
import qualified Data.Set                        as S
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           Text.PrettyPrint.Mainland.Class (Pretty (ppr))


import           Language.SMEIL.Syntax
import           Language.SMEIL.Util
import           SME.Error
import           SME.Util

--import           Debug.Trace                     (trace, traceM)
traceS :: String -> a -> a
traceS _ = id

traceMS :: (Applicative f) => String -> f ()
traceMS _ = pure ()

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

-- | Type checking monad
class ( Monad m
      , MonadState (BaseEnv s) m
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
      (traceS
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
        traceMS ("Symbol table " ++ show (nameOf d))
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
                traceS
                  "Got back InstDef "
                  withScope
                  instantiated
                  (go (N.fromList is))
              ParamDef {paramType = BusPar {..}} ->
                traceS "GOt back ParamDef" $
                case ref of
                  (_ :| []) ->
                    throw $
                    InternalCompilerError "Bus reference is a single name"
                  (r' :| rs') -> withScope r' (go (N.fromList (rs' ++ is)))
              _
              -- If first name component doesn't resolve to a possible compound
              -- name in current scope, it probably refers to a top-level
              -- construct, so we look again in that
               -> traceS "lookup recursing" $ withScope i (go (N.fromList is))
          Nothing ->
            traceS "lookup recursing2" $ withScope i (go (N.fromList is))
  {-# INLINEABLE lookupDef' #-}

  -- | Looks up a top-level definition
  lookupTopDef :: (References a, Located a, Pretty a) => a -> m (BaseTopDef s)
  lookupTopDef i =
    go $ traceS ("lookupTopDef called with " ++ show (refOf i)) (refOf i)
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
       = traceS "lookupTopDef" $ throw $ UndefinedName i

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

  -- | Given a global reference, applies the function to update the definition.
  updateDefM ::
       (References a, Located a, Pretty a)
    => a
    -> (BaseDefType s -> m (BaseDefType s))
    -> m ()
  updateDefM d f
    -- Lookup whole reference, lookup definition, then update topDef doing a map
    -- insert with the update definition
   = do
    def <- lookupDef d
    let top = N.head (refOf d)
        name = nameOf def
    res <- f def
    updateTopDef
      top
      (\x -> x {symTable = M.insert (toString name) res (symTable x)})

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

  curEnvIdent :: m Ident
  curEnvIdent = gets curEnv

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

  setUsedBus :: Ref -> (Ref, BusState) -> m ()
  setUsedBus r s = do
    cur <- gets curEnv
    traceMS ("setUsedbus " ++ show cur ++ " " ++ show r ++ " " ++ show s)
    updateCurEnv
      (\case
         p@ProcessTable {usedBuses = usedBuses} ->
           p {usedBuses = M.insertWith S.union r (S.singleton s) usedBuses}
         other -> other)

  getBusState :: Ref -> m (Maybe [(Ref, BusState)])
  getBusState r =
    M.lookup r . usedBuses <$> getCurEnv >>= \case
      Just s -> pure $ Just $ S.toList s
      Nothing -> pure Nothing

  withScope :: (References a, Pretty a, Located a) => a -> m b -> m b
  withScope s act = do
    cur <- gets curEnv
    new <- lookupTopDef s
    modify (\x -> x {curEnv = nameOf new})
    act <* modify (\x -> x {curEnv = cur})

  getCurEnv :: (MonadRepr a m) => m (BaseTopDef a)
  getCurEnv = do
    e <- gets curEnv
    traceMS ("getCurEnv " ++ show e)
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
    -> (BaseDefType s -> Maybe (BaseDefType s))
    -> m ()
  updateDef d f = do
    cur <- gets curEnv
    def <- lookupDef d
    traceMS ("In updateDef: " ++ show (nameOf def)) -- ++ " " ++ show cur)
    let def' = f def
    when (isJust def') $
      updateTopDef
        cur
        (\x ->
           x
           { symTable =
               M.insert (toString (nameOf def)) (fromJust def') (symTable x)
           })

  getConfig :: (MonadRepr s m) => (Config -> a) -> m a
  getConfig f = f <$> gets config

  getTypeCtx :: (MonadRepr s m) => m (Maybe Typeness)
  getTypeCtx = gets typeCtx

  withTypeCtx :: (MonadRepr s m) => Typeness -> m a -> m a
  withTypeCtx t act = do
    prev <- getTypeCtx
    modify (\x -> x { typeCtx = Just t })
    act <* modify (\x -> x { typeCtx = prev })

lookupEx ::
     (ToString a, Pretty a, Located a, MonadRepr s m)
  => a
  -> M.HashMap String b
  -> m b
lookupEx e m = case M.lookup (toString e) m of
  Just r  -> pure r
  Nothing -> traceS "lookupEx" $ throw $ UndefinedName e

-- | Tries to look up an element 'i' in map 'm'. Performs action 'a' if
-- successful and throws an error otherwise
ensureUndef ::
     (Nameable d, ToString a, Located a)
  => a
  -> M.HashMap String d
  -> m b
  -> m b
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
  deriving (Show, Data)

data Value
  = IntVal !Integer
  | ArrayVal !Int
             (Vector Value)
  | BoolVal !Bool
  | DoubleVal !Double
  | SingleVal !Float
  deriving (Show, Eq)

instance Ord Value
  -- This instance is "bad" as it makes an effort to implement a notion of
  -- type-unsafe and somewhat arbitrary comparisons between SMEIL
  -- values. However, should comparisons between values of different types ever
  -- happen, it should be considered a type-checker bug.
                                                         where
  (IntVal a) `compare` (IntVal b) = a `compare` b
  (IntVal a) `compare` (BoolVal False) = a `compare` 0
  (IntVal a) `compare` (BoolVal True) = a `compare` 1
  (IntVal a) `compare` (SingleVal b) = fromIntegral a `compare` b
  (IntVal a) `compare` (DoubleVal b) = fromIntegral a `compare` b
  (BoolVal a) `compare` (BoolVal b) = a `compare` b
  (BoolVal True) `compare` (IntVal b) = 1 `compare` b
  (BoolVal False) `compare` (IntVal b) = 0 `compare` b
  (BoolVal _) `compare` (SingleVal _) = GT -- TODO
  (BoolVal _) `compare` (DoubleVal _) = GT -- TODO
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

instance Pretty Value where
  ppr (IntVal i)     = ppr i
  ppr (ArrayVal _ v) = ppr (V.toList v)
  ppr (BoolVal b)    = ppr b
  ppr (DoubleVal d)  = ppr d
  ppr (SingleVal s)  = ppr s

valueToLit :: Value -> Literal
valueToLit (IntVal i)      = LitInt i noLoc
valueToLit (DoubleVal i)   = LitFloat (realToFrac i) noLoc
valueToLit (SingleVal i)   = LitFloat (realToFrac i) noLoc
valueToLit (BoolVal True)  = LitTrue noLoc
valueToLit (BoolVal False) = LitFalse noLoc
valueToLit ArrayVal {}     = undefined

-- | For values that are instance of Num, runs abs on the value. Returns the
-- identity otherwise
absValue :: Value -> Value
absValue (IntVal v)    = IntVal $ abs v
absValue (DoubleVal v) = DoubleVal $ abs v
absValue (SingleVal v) = SingleVal $ abs v
absValue v             = v

mkVarDef :: Ident -> Typeness -> a -> BaseDefType a
mkVarDef i t el =
  VarDef
  { varName = i
  , varDef =
      Variable {name = i, ty = t, val = Nothing, range = Nothing, loc = noLoc}
  , varState = Used
  , varVal = Nothing
  , ext = el
  }

-- | Type for representing definitions in SMEIL
data BaseDefType a
  = VarDef { varName  :: Ident
           , varDef   :: Variable
           , varState :: VarState
           --, varVa
           , varVal   :: Maybe Literal
           -- , varRange :: (Integer, Integer)
           , ext      :: a }
  | ConstDef { constName  :: Ident
             , constDef   :: Constant
             , constState :: VarState
             , constVal   :: Literal
             , ext        :: a }
  | BusDef { busName   :: Ident
           , busRef    :: Ref -- ^ The global reference to the bus declaration
           , busShape  :: BusShape
           , busDef    :: Bus
           , busState  :: BusState
           , shared    :: BusVisibility
             -- ^ Property indicating if a bus is used for communicating outside
             -- of an instance. Starts out as true. Flips to false if a bus is
             -- read from and written to in the same cycle. Accessing a bus with
             -- shared = false from outside the process where it is declared is
             -- an error
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
            , params       :: [InstParam]
            , anonymous    :: Bool
            , ext          :: a
              -- Also track: Parameters
             }
  | ParamDef { paramName :: Ident
             , paramType :: ParamType
             , ext       :: a }
  deriving (Show, Data)

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

-- | Sets the type of a definition when possible
setType :: Typeness -> Maybe Value -> BaseDefType a -> Maybe (BaseDefType a)
setType t val d@VarDef {varDef = v@Variable {}} =
  let range' = mkRange $ valueToLit <$> val
  in Just $ d {varDef = v {ty = t, range = range'}}
setType _ _ _ = Nothing

-- TODO (maybe): If we alter the bus definition AST directly here, we don't have
-- to pass around the range in the BusShape
setBusChanType ::
     Ident -> Maybe Value -> Typeness -> BaseDefType a -> Maybe (BaseDefType a)
setBusChanType ident range ty b@BusDef {busShape = shape} =
  let shape' =
        BusShape $
        map
          (\o@(i, (_, l, _)) ->
             if ident == i
               then (i, (ty, l, valueToLit <$> range))
               else o)
          (unBusShape shape)
  in Just b {busShape = shape'}
setBusChanType _ _ _ _                                = Nothing

lookupTy ::
     (MonadRepr s m, References a, Located a, Pretty a) => a -> m Typeness
lookupTy r = do
  traceMS "LookupTy entered"
  (rest, def) <- lookupDef' r
  res <- setTypeLoc (locOf r) <$> getType rest def
  return $
    traceS ("lookupTy for " ++ show (refOf r) ++ " return " ++ show res) res
  where
    getType _ VarDef {..} = pure $ typeOf varDef
    getType _ ConstDef {..} = pure $ typeOf constDef
    getType [rest] BusDef {..} = fst3 <$> lookupBusShape busShape rest
    getType _ BusDef {} = traceS "throw busdef" $ throw $ BusReferencedAsValue r
    getType _ FunDef {} = undefined --pure $ typeOf funcDef
    getType _ EnumDef {..} = throw $ ReferencedAsValue enumDef
    getType _ EnumFieldDef {..} = pure $ typeOf fieldValue
    getType _ InstDef {..} = throw $ ReferencedAsValue instDef
    getType [] ParamDef {..} =
      case paramType of
        ConstPar t  -> pure t
        BusPar {..} -> throw $ BusReferencedAsValue r
    getType [rest] ParamDef {..} =
      case paramType of
        ConstPar _ -- TODO: Better error message
         -> traceS "ParamType " $ throw $ UndefinedName r
        BusPar {..} -> fst3 <$> lookupBusShape parBusShape rest
    getType _ ParamDef {} -- TODO: Better error message
     = traceS "ParamDef" $ throw $ UndefinedName r
    lookupBusShape busShape rest
      -- TODO: Maybe use a map for busShape
     =
      case lookup rest (unBusShape busShape) of
        Just a  -> pure a
        Nothing -> traceS "lookupBusShape" $ throw $ UndefinedName r


-- TODO: Maybe change this to a map
newtype BusShape = BusShape
  -- Tuple is, type, default val, range
  { unBusShape :: [(Ident, (Typeness, Maybe Literal, Maybe Literal))]
  } deriving (Show, Data)

instance Eq BusShape where
  a == b = dropRange a == dropRange b
    where
      dropRange = map (\(x, (y, z, _)) -> (x, y, z)) . unBusShape

--type BusShape = M.HashMap Ident (Typeness, Maybe Expr)

data InstParam
  = InstConstPar Expr
  | InstBusPar Ref
  deriving (Show, Eq, Data)

data ParamType
  = ConstPar Typeness
  | BusPar { ref         :: Ref
           -- ^ Reference to the bus declaration itself
           , localRef    :: Ref
           -- ^ The bus reference used within the instantiated process
           , parBusShape :: BusShape
           , busState    :: BusState
           , array       :: Maybe Integer}
  deriving (Show, Eq, Data)

data BusVisibility
  = Shared
  | Private
  deriving (Show, Eq, Data)

data BusState
  = Input
  | Output
  | Local
  | Unassigned
  deriving (Show, Eq, Ord, Data)

data VarState
  = Used
  | Unused
  deriving (Eq, Show, Data)

type BaseSymTab a = M.HashMap String (BaseDefType a)
type ParamList = [(Ident, ParamType)]

-- | Map from the global reference of a bus definition a set containing the
-- local name that is used for a bus within a process and the mode of the bus
-- (input/output/...)
type UsedBuses = M.HashMap Ref (S.Set (Ref, BusState))

data BaseTopDef a
  = ProcessTable { symTable  :: BaseSymTab a
                 , procName  :: Ident
                 -- FIXME: Consider removing params since the info already
                 -- exists in the symtable
                 , params    :: ParamList
                 --, params    :: M.HashMap Ident ParamType
                 , stms      :: [Statement]
                   --, usedBuses :: M.HashMap Ref (S.Set (Maybe Ident, BusState))
                 , usedBuses :: UsedBuses
                 --, referencedProcs ::
                 , procDef   :: Process
                 , ext       :: a }
  | NetworkTable { symTable :: BaseSymTab a
                 , netName  :: Ident
                 , params   :: ParamList
                 , netDef   :: Network
                 , topLevel :: Bool
                 , ext      :: a }
  deriving (Show, Data)

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
    BaseEnv ((f <$>) <$> defs) curEnv used typeCtx config (f ext)

-- | Top-level type checking environment
data BaseEnv a = BaseEnv
  { defs    :: M.HashMap String (BaseTopDef a) -- ^ Top-level symbol table
  --, curEnv :: BaseTopDef a -- ^ Scope of currently used definition
  , curEnv  :: Ident -- TODO: Probably change to Maybe Ref
  , used    :: S.Set Ident -- ^ Instantiated definitions
  , typeCtx :: Maybe Typeness -- TODO: Replace with a reader monad env
  , config  :: Config
  , ext     :: a
  } deriving (Show)

mkEnv :: Config -> a -> BaseEnv a
mkEnv = BaseEnv M.empty (Ident "__noEnv__" noLoc) S.empty Nothing

data Stages
  = ResolveImport
--  | Rename
  | TypeCheck
  | CodeGen
  | Retyped
  deriving (Eq, Show)

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
                  --, ("optimize", Optimize)
                  , ("code-generation", CodeGen)
                  , ("typed", Retyped)
                  ]) .
        map toLower

data Config = Config
  { inputFile          :: FilePath
  , outputDir          :: Maybe FilePath
  , dumpStages         :: [Stages] -- ^ Show the output of these stages
  , force              :: Bool -- ^ Overwrite files in preexisting directories
  , noStrictSizeBounds :: Bool -- ^ Assume all integers to be dynamically sized
  --, inferSizeBounds    :: Bool -- ^ Adjust sizes according to
  , quiet              :: Bool
  , noAsserts          :: Bool
  , emulateOverflows   :: Bool
  , runSim             :: Maybe Int
  , traceFile          :: Maybe FilePath
  , noWarnings         :: Bool -- ^ Are warnings disabled
  , noRangeAnnot       :: Bool
  , params             :: [String] -- [(String, [(String, String)])]
  -- ^ Entity parameters supplied as command line options.
  } deriving (Show)

mkConfig :: Config
mkConfig =
  Config
  { inputFile = ""
  , outputDir = Nothing
  , dumpStages = []
  , force = False
  , noStrictSizeBounds = False
  --, inferSizeBounds = False
  , quiet = False
  , noAsserts = False
  , emulateOverflows = False
  , runSim = Nothing
  , traceFile = Nothing
  , noWarnings = False
  , noRangeAnnot = False
  , params = []
  }
