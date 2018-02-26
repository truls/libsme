{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | Perform type checking and various static analyses of an SMEIL program
module SME.TypeCheck
  ( typeCheck
  ) where

import           Control.Arrow               (first)
import           Control.Exception           (Exception, SomeException, throw)
import           Control.Monad               (foldM, forM, forM_, unless,
                                              zipWithM)
import           Control.Monad.Except        (ExceptT, MonadError, catchError,
                                              runExceptT)
import           Control.Monad.Identity      (Identity, runIdentity)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State         (MonadState, StateT, gets, modify,
                                              runState, runStateT)
import           Control.Monad.Writer        (MonadWriter, WriterT, runWriterT)
import           Data.Generics.Uniplate.Data (universeBi)
import           Data.Hashable               (hash)
import qualified Data.HashMap.Strict         as M
import           Data.List                   (sortOn)
import           Data.Loc                    (Loc, Located (..), SrcLoc (..),
                                              noLoc)
import           Data.Maybe                  (fromMaybe, isNothing)
import qualified Data.Set                    as S

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.Error

import           Text.Show.Pretty            (ppShow)

--import           Debug.Trace                 (trace)
trace :: String -> a -> a
trace _ = id

-- * Type checking monad and data structures for holding type checking state

data TyLog =
  Inf Info
  | Warn Warning
  deriving (Show)

newtype Info =
  TypeFromRange String
  deriving (Show)

newtype Warning =
  TypeCheckWarning String
  deriving (Show)

type Log = [TyLog]

-- | Main typechecking monad
newtype TyM a = TyM
  { unTyM :: ExceptT TypeCheckErrors (WriterT Log (StateT Env Identity)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError TypeCheckErrors
             , MonadState Env
             , MonadWriter Log
             )

-- | Type checking monad
class ( Monad m
      -- , MonadState Env m
      -- , MonadWriter Log m
      -- , MonadError TypeCheckErrors m
      ) =>
      MonadTypeCheck m where
  lookupDef' :: (References a) => a -> m ([Ident], DefType)
  lookupDef :: (References a) => a -> m DefType
  lookupTopDef :: (References a) => a -> m TopDef
  updateTopDef :: (References a) => a -> (TopDef -> TopDef) -> m ()
  mapDefs :: (References a) => a -> (DefType -> m DefType) -> m ()
  forUsedTopDefs :: (TopDef -> m ()) -> m ()
  --withScope :: (ToString a, Located a) => a -> m b -> m b
  withScope :: (References a) => a -> m b -> m b
  addDefinition :: (References a, Located b, ToString b) => a -> b -> DefType -> m ()
  addTopDef :: TopDef -> m ()
  addUsedEnt :: (Nameable a) => a -> m ()

  --lookupInstance :: String -> m [DefType]
  --lookupInstances :: m [DefType]

lookupOrFail ::
     (ToString a, Pretty a, Located a, MonadTypeCheck m)
  => a
  -> M.HashMap String b
  -> m b
lookupOrFail e m = case M.lookup (toString e) m of
  Just r  -> pure r
  Nothing -> throw $ UndefinedName e

instance MonadTypeCheck TyM where
  lookupDef r = do
    (_, res) <- lookupDef' r
    return res
  lookupDef' r = go (trace ("Lookup called with " ++ show (refOf r)) (refOf r))
      -- Three cases.
      -- Case one: Single name [Ident] is found in local scope. Return that
      -- Case two: First name [Ident, ..] refers to a bus in local scope. Return
      -- that.
      -- Case three: Name [Ident, ...] refers to a top-level name. Switch
      -- scope, remaining name compounds should fall into previous cases
    where
      go [] = throw $ InternalCompilerError "Empty reference."
      go [i] = do
        d <- gets curEnv
        res <- lookupOrFail i (symTable d)
        return ([], res)
      go (i:is) = do
        d <- gets curEnv
        lookupOrFail i (symTable d) >>= \case
          b@BusDef {} -> pure (is, b)
          InstDef {instantiated = instantiated} ->
            withScope instantiated (go is)
          ParamDef {paramType = BusPar {..}} ->
            case ref of
              (r':rs') -> withScope r' (go (rs' ++ is))
              _ ->
                throw $ InternalCompilerError "Bus reference is a single name"
          _ ->
            error
              ("TODO: Proper error, Name is not of an indexable type " ++ show i) `catchError`
            (\_ -> withScope i (go is))
  lookupTopDef i =
    go $ trace ("lookupTopDef called with " ++ show (refOf i)) (refOf i)
    where
      go [ident] = do
        ds <- gets defs
        lookupOrFail ident ds >>= \case
          EmptyTable ->
            throw $ InternalCompilerError "Lookup returned empty table"
          e -> pure e
      go _ =
        throw $
        InternalCompilerError
          "Compound name should not be present at this point"
  mapDefs d f = do
    def <- lookupTopDef d
    let tab = symTable def
        ks = M.keys tab
        defs = M.elems tab
    res <- mapM f defs
    updateTopDef d (\x -> x {symTable = M.fromList (zip ks res)})
  forUsedTopDefs f = do
    u <- gets used
    mapM_
      (\x -> do
         def <- lookupTopDef x
         f def)
      u
    --modify (\x -> x { defs = M.fromList (zip ks defs) })
  updateTopDef i f = do
    def <- lookupTopDef i
    modify (\x -> x {defs = M.insert (toString (nameOf def)) (f def) (defs x)})
  withScope s act = do
    cur <- gets curEnv
    e <- lookupTopDef s
    modify (\x -> x {curEnv = e})
    act <* modify (\x -> x {curEnv = cur})
  addDefinition topDef k v = do
    def <- lookupTopDef topDef
    symTab <-
      ensureUndef k (symTable def) $
      pure $ M.insert (toString k) v (symTable def)
    updateTopDef topDef (\x -> x {symTable = symTab})
  addTopDef d = do
    ds <- gets defs
    let n = toString $ nameOf d
    ensureUndef (nameOf d) ds $ modify (\s -> s {defs = M.insert n d (defs s)})
  addUsedEnt e = modify (\x -> x {used = S.insert (nameOf e) (used x)})
  --lookupInstance = undefined
  --lookupInstances = undefined

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
data DefType
  = VarDef { varName :: Ident
           , varDef  :: Variable }
  | ConstDef { constName :: Ident
             , constDef  :: Constant }
  | BusDef { busName  :: Ident
           , busRef   :: [Ident]
           , busShape :: BusShape
           , busDef   :: Bus }
  | FunDef { funcName :: Ident
           , funcDef  :: Function }
  | EnumDef { enumName   :: Ident
            , enumFields :: [(Ident, Integer)]
            , enumDef    :: Enumeration }
  | EnumFieldDef { fieldName  :: Ident
                 , fieldValue :: Integer
                 , defIn      :: Ident }
  | InstDef { instName     :: Ident
            , instDef      :: Instance
            , instantiated :: Ident
            , params       :: [(String, Value)]
            , anonymous    :: Bool
              -- Also track: Parameters
             }
  | ParamDef { paramName :: Ident
             , paramType :: ParamType }
  deriving (Show)

instance Nameable DefType where
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

data TopDef
  = ProcessTable { symTable :: M.HashMap String DefType
                 , procName :: Ident
                 , params   :: M.HashMap String ParamType
                 , stms     :: [Statement]
                 , procDef  :: Process
                 }
  | NetworkTable { symTable :: M.HashMap String DefType
                 , netName  :: Ident
                 , params   :: M.HashMap String ParamType
                 , netDef   :: Network }
  | EmptyTable
  deriving (Show)

instance Nameable TopDef where
  nameOf ProcessTable {..} = procName
  nameOf NetworkTable {..} = netName
  nameOf EmptyTable        = Ident "" noLoc

instance Located TopDef where
  locOf ProcessTable {..} = locOf procDef
  locOf NetworkTable {..} = locOf netDef
  locOf EmptyTable {}     = noLoc

-- | Top-level type checking environment
data Env = Env
  { defs   :: M.HashMap String TopDef -- ^ Top-level symbol table
  , curEnv :: TopDef -- ^ Scope of currently used definition
  , used   :: S.Set Ident -- ^ Instantiated definitions
  } deriving (Show)

mkEnv :: Env
mkEnv = Env M.empty EmptyTable S.empty

lookupBus :: (References a, MonadTypeCheck m) => a -> m DefType
lookupBus r =
  let ref = trace ("lookupBus called with " ++ show (refOf r)) (refOf r)
  in lookupDef r >>= \case
       b@BusDef {} -> pure b
         -- FIXME: Partial function head
       _ -> throw $ ExpectedBus (head ref)

lookupTy ::
     (MonadTypeCheck m, References a, Located a, Pretty a) => a -> m Typeness
lookupTy r = do
  (rest, def) <- lookupDef' r
  res <- setTypeLoc (locOf r) <$> getType rest def
  return $ trace ("lookupTy for " ++ show (refOf r) ++ " return " ++ show res) res
  where
    getType _ VarDef {..} = pure $ typeOf varDef
    getType _ ConstDef {..} = pure $ typeOf constDef
    getType [rest] BusDef {..} = lookupBusShape busShape rest
    getType _ BusDef {} -- TODO: Better error message
     = throw $ UndefinedName r
    getType _ FunDef {} = undefined --pure $ typeOf funcDef
    getType _ EnumDef {..} = throw $ ReferencedAsValue enumDef
    getType _ EnumFieldDef {..} = pure $ typeOf fieldValue
    getType _ InstDef {..} = throw $ ReferencedAsValue instDef
    getType [] ParamDef {..} =
      case paramType of
        ConstPar t  -> pure t
        BusPar {..} -> error "TODO: Make proper error. Bus definition referenced as value." -- throw $ ReferencedAsValue
    getType [rest] ParamDef {..} =
      case paramType of
        ConstPar _  -> -- TODO: Better error message
          throw $ UndefinedName r
        BusPar {..} -> lookupBusShape busShape rest
    getType _ ParamDef {} -- TODO: Better error message
     = throw $ UndefinedName r

    lookupBusShape busShape rest =
      let BusShape bs = busShape
      -- TODO: Maybe use a map for busShape
      in case lookup rest bs of
           Just a  -> pure a
           Nothing -> throw $ UndefinedName r

unifyTypes ::
     (MonadTypeCheck m) => Maybe Type -> Typeness -> Typeness -> m Typeness
unifyTypes expected t1 t2 = do
  res <- go t1 t2
  case expected of
    Just t -> do
      unless (Typed t == res) $ throw $ TypeMismatchError (Typed t) res
      return (Typed t)
    Nothing -> return res
  where
    go Untyped Untyped         = error "Untyped" -- pure Untyped
    go Untyped t               = pure t
    go t Untyped               = pure t
    go (Typed t1') (Typed t2') = Typed <$> go' t1' t2'
    -- go t1' t2' =
    --   if t1' == t2'
    --     then pure t1'
    --     else throw $ TypeMismatchError t1' t2'
    go' (Unsigned l1 loc) (Unsigned l2 _) = return $ Unsigned (max l1 l2) loc
    go' (Signed l1 loc) (Signed l2 _) = return $ Signed (max l1 l2) loc
    go' (Unsigned l1 loc) (Signed l2 _) -- FIXME: Does this formula makes sense?
     =
      return $
      Signed
        (max l1 l2 +
         (if l1 >= l2
            then 1
            else 0))
        loc
    go' (Signed l1 loc) (Unsigned l2 _) =
      return $
      Signed
        (max l1 l2 +
         (if l2 >= l1
            then 1
            else 0))
        loc
    go' t1'@(Array l1 ity1 _loc) t2'@(Array l2 ity2 loc) = do
      (Typed ity) <- unifyTypes Nothing (Typed ity1) (Typed ity2)
      unless (l1 == l2) $ throw $ TypeMismatchError (Typed t1') (Typed t2')
      return (Array l1 ity loc)
    go' (Bool loc) (Bool _) = return (Bool loc)
    go' t1' t2' = throw $ TypeMismatchError (Typed t2') (Typed t1')

flipSignedness :: Typeness -> Typeness
flipSignedness (Typed t) = Typed $ go t
  where
    go (Signed l loc) =
      Unsigned
        (if l > 2
           then l - 1
           else l)
        loc
    go (Unsigned l loc) = Signed (l + 1) loc
    go t1 = t1
flipSignedness Untyped = Untyped

setTypeLoc :: Loc -> Typeness -> Typeness
setTypeLoc _ Untyped     = Untyped
setTypeLoc loc (Typed t) = Typed (t { loc = SrcLoc loc } :: Type)

-- | Check expression and update references as needed
checkExpr :: (MonadTypeCheck m) => Expr -> m (Typeness, Expr)
checkExpr p@PrimName {..} = do
  ty' <- lookupTy name
  return (ty', p { ty = ty' } :: Expr)
checkExpr p@FunCall {..}  = (,p) <$> lookupTy name
checkExpr p@PrimLit {..} =
  let t = typeOf lit

  in return (t, p {ty = t} :: Expr)
checkExpr Binary {..}   = do
  (t1, e1) <- checkExpr left
  (t2, e2) <- checkExpr right
  t' <- unifyTypes Nothing t1 t2
  return (t', Binary t' binOp e1 e2 loc)
checkExpr Unary {..} = do
  (t', e') <- checkExpr expr
  let t'' = flipSignedness t'
  return (t'', Unary t'' unOp e' loc)

-- | Check statements and update references as needed
checkStm :: (MonadTypeCheck m) => Statement -> m Statement
checkStm Assign {..} = do
  destTy <- lookupTy dest
  (t, val') <- checkExpr val
  _<- unifyTypes Nothing destTy t
  return $ Assign dest val' loc
checkStm If {..} = do
  (condTy, cond') <- checkExpr cond
  -- TODO: Replace this with an explicit expectation parameter for checkExpr
  _ <- unifyTypes Nothing condTy (Typed (Bool noLoc))
  body' <- mapM checkStm body
  elif' <- mapM (\(e, stms) -> do
                    (elifCondTy, e') <- checkExpr e
                    _ <- unifyTypes Nothing elifCondTy (Typed (Bool noLoc))
                    stms' <- mapM checkStm stms
                    return (e', stms')
                ) elif
  els' <- case els of
    Just stms -> Just <$> mapM checkStm stms
    Nothing   -> pure Nothing
  return $ If cond' body' elif' els' loc
checkStm For {..} = do
  --varTy <- lookupTy var
  -- TODO: Bind variable in environment typed depending on ranges of expresions
  (_fromTy, from') <- checkExpr from
  -- TODO: Unless fromTy is int
  (_toTy, to') <- checkExpr to
  -- TODO: Unless toTy is into
  body' <- mapM checkStm body
  return $ For var from' to' body' loc
checkStm Switch {..} = do
  (_valTy, value') <- checkExpr value
  cases' <- mapM (\(e, stms) -> do
                     stms' <- mapM checkStm stms
                     return (e, stms')
                 ) cases
  -- TODO: Make sure that the types of all the cases checks out and matches type
  -- of value
  defaultCase' <- case defaultCase of
    Just stms -> Just <$> mapM checkStm stms
    Nothing   -> pure Nothing
  return $ Switch value' cases' defaultCase' loc
checkStm Barrier {..} = return $ Barrier loc
checkStm Break {..} = return $ Break loc
checkStm Return {..} = do
  -- TODO: make sure that context is a function and make sure that return value
  -- matches declared return value of function. Maybe use a context stack in env
  -- to keep track of this
  (_, retVal') <- case retVal of
    Just e -> do
      (retTy, e') <- checkExpr e
      return (retTy, Just e')
    Nothing -> return (Untyped, Nothing)
  return $ Return retVal' loc

-- | Check and annotate the all topLevel definitions
checkDef :: (MonadTypeCheck m) => DefType -> m DefType
checkDef v@VarDef {..} = do
  -- Check that range is within type constraint
  res <- go varDef
  return $ v {varDef = res}
  where
    -- TODO: Actually check that a) default value, if existing, is within type
    -- and range constraints and that range constraints, if existing, is within
    -- type bounds
    go var@Variable {..} = pure var
checkDef c@ConstDef {..} = do
  res <- go constDef
  return $ c { constDef = res }
  where
    go constant@Constant {..} = pure constant
checkDef b@BusDef {busDef = busDef} = do
  res <- go busDef
  return $ b {busDef = res}
  where
    go bd@Bus {signals = signals} = do
      signals' <-
        forM
          signals
          (\bsig@BusSignal {..} ->
             -- TODO: Check bus signal declared type against default value and range
             return bsig)
      return $ bd {signals = signals'}

checkDef FunDef {..} = error "TODO: Function declarations unhandeled"
  -- TODO:
  -- do
  -- res <- go funcDef
  -- where
  --   go
-- Let's keep matching exhaustiveness check
checkDef e@EnumDef {} =
  -- Enum defs are already correctly typed by the buildProcTab function
  return e
checkDef ef@EnumFieldDef {} =
  -- Same as above
  return ef
checkDef i@InstDef {} =
  -- Instance definitions are handled separately
  return i
checkDef p@ParamDef {} =
  return p



checkTopDef :: (MonadTypeCheck m) => TopDef -> m ()
checkTopDef ProcessTable { ..} = do
  mapDefs procName checkDef
  body' <- withScope procName $ mapM checkStm stms
  updateTopDef procName  (\x -> x { stms = body'} )
checkTopDef NetworkTable {..} =
  updateTopDef netName id
checkTopDef EmptyTable = return ()

-- | Tries to look up an element 'i' in map 'm'. Performs action 'a' if
-- successful and throws an error otherwise
ensureUndef ::
     (MonadTypeCheck m, Nameable d, ToString a, Located a) => a -> M.HashMap String d -> m b -> m b
ensureUndef i m a =
  case M.lookup (toString i) m of
    Just r  -> throw $ DuplicateName i (nameOf r)
    Nothing -> a

ensureSingleRef :: (References a) => a -> Ident
ensureSingleRef r = go $ refOf r
  where
    go [i] = i
    go _ = throw $ InternalCompilerError "Compound names should not occur at this point"

-- | Create an environment for a process by adding its definitions
buildDeclTab :: (MonadTypeCheck m) => Ident -> [Declaration] -> m (M.HashMap String DefType)
buildDeclTab ctx = foldM go M.empty
  where
    go m (VarDecl v@Variable {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (VarDef name v) m
    go m (ConstDecl c@Constant {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (ConstDef name c) m
    go m (BusDecl b@Bus {..}) =
      ensureUndef name m $
      return $
      M.insert (toString name) (BusDef name [ctx, name] (busToShape b) b) m
    go m (FuncDecl f@Function {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (FunDef name f) m
    go m (EnumDecl e@Enumeration {..})
      -- Assign numbers to unnumbered enum fields in a similar fashion to C. If
      -- the first field f_0 is not assigned a value, it will be given the value
      -- 0, then subsequent fields f_1 will be given the value 1. If a field f_n
      -- is explicitly assigned the value n, the subsequent fields f_{n+1} will
      -- be assigned the value n + 1
     = do
      enumFields <- fillEnum 0 fields
      let maxVal = maximum (map snd enumFields)
      res <-
        ensureUndef name m $
        return $
        M.insert
          (toString name)
          (EnumDef name enumFields (e {ty = typeOf maxVal}))
          m
      -- Insert fields of enum as entries into the entity symbol tab
      foldM
        (\m' (f, v) ->
           ensureUndef f m' $
           return $ M.insert (toString f) (EnumFieldDef f v name) m')
        res
        enumFields
      where
        fillEnum n ((ident, expr):restFields) = do
          expr' <-
            case expr of
              Just num -> exprReduceToInt num
              Nothing  -> pure n
          rest <- fillEnum (expr' + 1) restFields
          return $ (ident, expr') : rest
        fillEnum _ [] = return []
    go m (InstDecl i@Instance {..}) =
      let name =
            fromMaybe
            -- Identifiers cannot start with _ so everything starting with _ is
            -- reserved for our private namespace
              (Ident ("_anonymous_" ++ toString (nameOf i)) noLoc)
              instName
          isAnon = isNothing instName
      in ensureUndef name m $
         return $
         M.insert
           (toString name)
           (InstDef name i (ensureSingleRef elName) [] isAnon)
           m
    go _ GenDecl {} = error "TODO: Generator declarations are unhandeled"

buildProcTab :: (MonadTypeCheck m) => Process -> m TopDef
buildProcTab p@Process {name = n, decls = d, body = body} = do
  tab <- buildDeclTab n d
  return $ ProcessTable tab (nameOf p) M.empty body p

buildNetTab :: (MonadTypeCheck m) => Network -> m TopDef
buildNetTab net@Network {name = n, netDecls = d} = do
  tab <- foldM go M.empty d
  return $ NetworkTable tab n M.empty net
  where
    go m (NetConst c@Constant {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (ConstDef name c) m
    go m (NetBus b@Bus {..}) =
      ensureUndef name m $
      return $
      M.insert (toString name) (BusDef name [n, name] (busToShape b) b) m
    go m (NetInst i@Instance {..}) =
      let name
            -- Identifiers cannot start with _ so we have those names reserved
            -- for our private namespace. Prefix with __ to avoid clashing with
            -- names introduced by the import handler
           =
            fromMaybe
              (Ident ("__anonymous_" ++ pprr elName) noLoc)
              instName
          isAnon = isNothing instName
      in ensureUndef name m $
         return $
         M.insert
           (toString name)
           (InstDef name i (ensureSingleRef elName) [] isAnon)
           m
         -- TODO: Maybe we can handle indexed instances by adding a separate
         -- DefType entry IndexedInstDef
    go _ NetGen {} = error "TODO: Generator declarations are unhandeled"

-- | Reduce an expression to a name and fail if impossible
exprReduceToName :: (MonadTypeCheck m) => Expr -> m Name
exprReduceToName PrimName {..} = return name
exprReduceToName e             = throw $ ExprInvalidInContext e

exprReduceToInt :: (MonadTypeCheck m) => Expr -> m Integer
-- TODO: Run expression through simplifier attempting to evaluate it to a
-- constant int
exprReduceToInt (PrimLit _ (LitInt i _) _) = return i
exprReduceToInt e                          = throw $ ExprInvalidInContext e

busToShape :: Bus -> BusShape
busToShape Bus {signals = signals} = BusShape $ map go signals
  where
    go BusSignal {..} = (name, ty)

groupWithM ::
     (Monad m, Eq a)
  => (b -> b -> m b)
  -> [(a, b)]
  -> m [(a, b)]
groupWithM _ [] = return []
groupWithM f ((x, y):xs)= do
  let (start, end) = span (\m -> fst m == x) ((x, y):xs)
  res <- foldM f y (map snd start)
  rest <- groupWithM f end
  return $ (x, res) : rest

unifyProcParam ::
     (MonadTypeCheck m, Located l)
  => (l, [(Ident, ParamType)])
  -> (l, [(Ident, ParamType)])
  -> m (l, [(Ident, ParamType)])
unifyProcParam (_, ps1) (inst, ps2) = do
  res <- zipWithM go ps1 ps2
  return (inst, res)
  where
    go (i1, ConstPar t1) (_, ConstPar t2) = do
      res <- unifyTypes Nothing t1 t2
      trace ("Unified ( " ++ show i1 ++ " types " ++ show t1 ++ " and " ++ show t2 ++ " yielding " ++ show res) $ return (i1, ConstPar res)
    go (i1, t1@BusPar {}) (_, t2@BusPar {}) =
      trace ("Comparing " ++ show t1 ++ " and " ++ show t2) $
      unless (t1 == t2) (throw $ BusShapeMismatch t1 t2 inst) >> return (i1, t1)
    go _ _ = throw $ InstanceParamTypeMismatch inst

-- | Infer the types of process parameters by checking how they are instantiated
-- and normalize instance parameter list to a <name>: <value> format
inferParamTypes ::
     (MonadTypeCheck m)
  => [(Ident, Instance)]
  -> m [(Ident, [(Ident, ParamType)])]
inferParamTypes insts = do
  instTypes <-
    forM insts (\(instCtxName, inst) -> (withScope instCtxName . go) inst)
  params <- groupWithM unifyProcParam $ sortOn fst instTypes
  return $ map (\(i, (_, pars)) -> (i, pars)) params
  where
    go i@Instance {elName = instantiated, params = formal} = do
      el <- lookupTopDef instantiated
      let actual = getParams el
          elName = nameOf el
      -- Add entity to the set of used entities
      -- TODO: This is a bit of hack. Unused entities should be filtered away by
      -- the set of used entities.
      addUsedEnt el
      unless
        (length formal == length actual)
        (throw $ ParamCountMismatch (length actual) (length formal) i elName)
      -- Generate list of inferred parameter types for this instance
      (elName, ) . (i, ) <$>
        zipWithM
          (\Param {count = count, dir = forDir, name = forName} (ident, e)
            -- If name is unnamed, substitute matching name to normalize
            -- parameter listings. If name is specified, make sure that the name
            -- used in the actual parameter matches the name of the formal parameter
            -> do
             let actualName = fromMaybe forName ident
             unless (forName == actualName) $
               throw $ NamedParameterMismatch forName actualName
              -- For every parameter, return a name, type tuple.
             case forDir of
               Const _ -> do
                 (_, eTy) <- checkExpr e
                 pure (forName, ConstPar (typeOf eTy))
               In _ -> mkBusDef Input e forName count
               Out _ -> mkBusDef Output e forName count)
          actual
          formal
    mkBusDef dir e forName count = do
      count' <-
        case count of
          Just (Just expr) -> exprReduceToInt expr >>= pure . Just
          _                -> pure Nothing
      bDef <- exprReduceToName e >>= lookupBus
                -- Derive common shape of bus here
      return
        ( forName
        , BusPar
          { ref = busRef bDef
          , busShape = busToShape (busDef bDef) -- TODO: Get the global
                       -- reference to a
          , busState = dir
          , array = count'
          })
                                -- bus here. Find some way of identifying buses
                                -- by matching their shapes/field names
    getParams ProcessTable {procDef = procDef} =
      (params :: Process -> [Param]) procDef
    getParams NetworkTable {netDef = netDef} =
      (params :: Network -> [Param]) netDef
    getParams EmptyTable = []

-- | Populate typechecking environment with all functions defined.
buildEnv :: (MonadTypeCheck m) => DesignFile -> m ()
buildEnv df = do
  let nets = universeBi df :: [Network]
      procs = universeBi df :: [Process]
  procs' <- mapM buildProcTab procs
  nets' <- mapM buildNetTab nets
  let topDefs = procs' ++ nets'
  -- TODO: Add an expand generators step here where we replace all generate
  -- statements with instances embedding their information and normalizing
  -- instance declaration for subsequent steps.
  mapM_ addTopDef topDefs
  let insts =
        concatMap
          (\x ->
             case x of
               EmptyTable -> []
               NetworkTable {..} ->
                 (netName, ) <$> (universeBi netDef :: [Instance])
               ProcessTable {..} ->
                 (procName, ) <$> (universeBi procDef :: [Instance]))
          topDefs
  params <- inferParamTypes insts
  trace (ppShow params) $
    forM_
      params
      (\(i, t) -> do
         updateTopDef
           i
           (\x ->
              case x of
                EmptyTable         -> EmptyTable
                nt@NetworkTable {} -> nt {params = toMap t} :: TopDef
                pc@ProcessTable {} -> pc {params = toMap t} :: TopDef)
         forM_
           t
           (\(n, param) ->
              trace
                ("Added definition " ++
                 show i ++ " " ++ show n ++ " " ++ show param)
                addDefinition
                i
                n
                (ParamDef n param)))
  forUsedTopDefs checkTopDef
  where
    toMap els = M.fromList (map (first toString) els)


-- | Do typechecking of an environment. Return DesignFile with completed type
-- annoations
typeCheck :: (MonadIO m) => DesignFile -> m DesignFile
typeCheck df = do
  let ((_,w), e) = runIdentity $ runStateT (runWriterT (runExceptT $ unTyM go)) mkEnv
  liftIO $ putStrLn $ ppShow e
  liftIO $ putStrLn $ ppShow w
  -- case r of
  --   Right res -> liftIO $ putStrLn e
  --   Left _    -> error "Left"
  return df
  where
    go =  buildEnv df
