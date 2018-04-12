{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | Perform type checking and various static analyses of an SMEIL program
module SME.TypeCheck
  ( typeCheck
  ) where

import           Control.Arrow               (first)
import           Control.Exception           (throw)
import           Control.Monad               (foldM, forM, forM_, unless,
                                              zipWithM)
import           Control.Monad.Except        (MonadError)

import           Control.Monad.Identity      (Identity)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Writer        (MonadWriter, WriterT, runWriterT,
                                              tell)
import           Data.Generics.Uniplate.Data (universeBi)
import qualified Data.HashMap.Strict         as M
import           Data.List                   (mapAccumR, sortOn)
import qualified Data.List.NonEmpty          as N
import           Data.Loc                    (Loc, Located (..), SrcLoc (..),
                                              fromLoc, noLoc)
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Monoid                 ((<>))

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.Error
import           SME.Representation
import           SME.Warning

import           Text.Show.Pretty            (ppShow)

--import           Debug.Trace                 (trace, traceM)
trace :: String -> a -> a
trace _ = id

traceM :: (Applicative f) => String -> f ()
traceM _ = pure ()

-- * Type checking monad and data structures for holding type checking state

type Env = BaseEnv Void
type DefType = BaseDefType Void
type TopDef = BaseTopDef Void
type SymTab = BaseSymTab Void

-- data TyLog =
--   Inf Info
--   | Warn Warning
--   deriving (Show)

-- newtype Info =
--   TypeFromRange String
--   deriving (Show)

-- newtype Warning =
--   TypeCheckWarning String
--   deriving (Show)

--type Log = [TyLog]
type Log = [TypeCheckWarning]

-- | Main typechecking monad
newtype TyM a = TyM
  { unTyM :: WriterT Log (ReprM Identity Void) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter Log
             , MonadState Env
             , MonadError TypeCheckErrors
             )

instance (MonadRepr Void) TyM

data AccessType
  = Store
  | Load
  deriving Eq

lookupBus :: (References a, MonadRepr Void m) => a -> m DefType
lookupBus r =
  let ref = trace ("lookupBus called with " ++ show (refOf r)) (refOf r)
  in lookupDef r >>= \case
       b@BusDef {} -> pure b
       _ -> throw $ ExpectedBus (N.head ref)

lookupTy ::
     (MonadRepr s m, References a, Located a, Pretty a) => a -> m Typeness
lookupTy r = do
  traceM "LookupTy entered"
  (rest, def) <- lookupDef' r
  res <- setTypeLoc (locOf r) <$> getType rest def
  return $
    trace ("lookupTy for " ++ show (refOf r) ++ " return " ++ show res) res
  where
    getType _ VarDef {..} = pure $ typeOf varDef
    getType _ ConstDef {..} = pure $ typeOf constDef
    getType [rest] BusDef {..} = fst <$> lookupBusShape busShape rest
    getType _ BusDef {} = trace "throw busdef" $ throw $ BusReferencedAsValue r
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
         -> trace "ParamType " $ throw $ UndefinedName r
        BusPar {..} -> fst <$> lookupBusShape busShape rest
    getType _ ParamDef {} -- TODO: Better error message
     = trace "ParamDef" $ throw $ UndefinedName r
    lookupBusShape busShape rest =
      -- TODO: Maybe use a map for busShape
      case lookup rest (unBusShape busShape) of
        Just a  -> pure a
        Nothing -> trace "lookupBusShape" $ throw $ UndefinedName r


-- | Tracks and update the usage of a variable in a scope and throws if
-- inconsistencies are found
trackUsage ::
     (MonadRepr s m, References a, Located a, Pretty a)
  => AccessType
  -> a
  -> (a -> m b)
  -> m b
trackUsage t r act
  --traceM $ ("In track usage: " ++ show (nameOf def))
  -- TODO: Track function usage also
 =
  lookupDef r >>= \case
    ConstDef {} -> do
      unless (t == Load) $ throw $ WroteConstant r
      updateDef r (\x -> x {constState = Used})
      act r
    VarDef {} -> do
      updateDef r (\x -> x {varState = Used})
      act r
    BusDef {busRef = bRef} -> do
      getBusState bRef >>= \case
        Nothing ->
          setUsedBus
            bRef
            -- TODO: We have to make sure that the reference we insert here
            -- reflects how the bus is actually referenced within the process
            ( refOf $ N.head bRef
            , if t == Load
                then Input
                else Output)
        Just a ->
          forM_
            (map (t, ) a)
            (\case
               (Load, (_, Input)) -> return ()
               (Load, _) -> throw $ ReadOutputBus r
               (Store, (_, Output)) -> return ()
               (Store, _) -> throw $ WroteInputBus r)
      act r
    _ -> act r


unifyTypes ::
     (MonadRepr s m) => Maybe Type -> Typeness -> Typeness -> m Typeness
unifyTypes expected t1 t2 = do
  res <- go t1 t2
  case expected of
    Just t -> do
      unifyTypes Nothing (Typed t) res
      -- unless (Typed t == res) $ throw $ TypeMismatchError (Typed t) res
      -- return (Typed t)
    Nothing -> return res
  where
    go Untyped Untyped         = error "Untyped" -- TODO: Better error
    go Untyped t               = pure t
    go t Untyped               = pure t
    go (Typed t1') (Typed t2') = Typed <$> go' t1' t2'
    go' (Unsigned l1 loc) (Unsigned l2 _) = return $ Unsigned (max l1 l2) loc
    go' (Signed l1 loc) (Signed l2 _) = return $ Signed (max l1 l2) loc
    go' (Unsigned (Just l1) loc) (Signed (Just l2) _) -- FIXME: Does formula below makes sense?
     =
      return $
      Signed
        (Just
           (max l1 l2 +
            (if l1 >= l2
               then 1
               else 0)))
        loc
    go' (Signed (Just l1) loc) (Unsigned (Just l2) _) =
      return $
      Signed
        (Just
           (max l1 l2 +
            (if l2 >= l1
               then 1
               else 0)))
        loc
    go' t1'@(Array l1 ity1 _loc) t2'@(Array l2 ity2 loc) =
      unifyTypes Nothing (Typed ity1) (Typed ity2) >>= \case
        (Typed ity) -> do
          unless (l1 == l2) $ throw $ TypeMismatchError (Typed t1') (Typed t2')
          return (Array l1 ity loc)
        Untyped -> throw $ TypeMismatchError (Typed t2') (Typed t1')
    go' (Bool loc) (Bool _) = return (Bool loc)
    -- TODO: Support unsized integers
    go' t1' t2' = throw $ TypeMismatchError (Typed t2') (Typed t1')

-- | Flips signed to unsigned and vice-versa
flipSign :: Typeness -> Typeness
flipSign (Typed t) = Typed $ go t
  where
    -- FIXME: Should we only flip signed -> unsigned?
    go (Signed (Just l) loc) =
      Unsigned (Just
        (if l > 2
           then l - 1
           else l))
        loc
    go (Unsigned (Just l) loc) = Signed (Just (l + 1)) loc
    go t1 = t1
flipSign Untyped = Untyped

-- | Sets the location of type
setTypeLoc :: Loc -> Typeness -> Typeness
setTypeLoc _ Untyped     = Untyped
setTypeLoc loc (Typed t) = Typed (t { loc = fromLoc loc } :: Type)

-- | Check expression and update references as needed
checkExpr :: (MonadRepr s m) => Expr -> m (Typeness, Expr)
checkExpr p@PrimName {..} = do
  ty' <- trackUsage Load name lookupTy
  return (ty', p {ty = ty'} :: Expr)
checkExpr p@FunCall {..}  = (,p) <$> lookupTy name
checkExpr p@PrimLit {..} =
  let t = typeOf lit
  in return (t, p {ty = t} :: Expr)
checkExpr Binary {..} = do
  -- TODO: Check that types are supported for operands
  (t1, e1) <- checkExpr left
  (t2, e2) <- checkExpr right
  t' <- unifyTypes Nothing t1 t2
  -- let shouldBe =
  --       case binOp of
  --         DisOp l -> Just $ Typed (Bool l)
  --         ConOp l -> Just $ Typed (Bool l)
  --         EqOp l  -> Nothing
  --         OrOp l  -> Just $ Typed (
  --         LtOp l  -> Typed (Bool l)
  --         GtOp l  -> Typed (Bool l)
  --         LeqOp l -> Typed (Bool l)
  --         GeqOp l -> Typed (Bool l)

  let retTy =
        case binOp of
          EqOp l  -> Typed (Bool l)
          OrOp l  -> Typed (Bool l)
          LtOp l  -> Typed (Bool l)
          GtOp l  -> Typed (Bool l)
          LeqOp l -> Typed (Bool l)
          GeqOp l -> Typed (Bool l)
          _       -> t'
  return (retTy, Binary retTy binOp e1 e2 loc)
checkExpr Unary {..} = do
  (t', e') <- checkExpr right
  let t'' =
        case unOp of
          UnMinus _ -> flipSign t'
          NotOp l   -> Typed (Bool l)
          _         -> t'
  return (t'', Unary t'' unOp e' loc)

-- | Check statements and update references as needed
checkStm :: (MonadRepr s m) => Statement -> m Statement
checkStm Assign {..} = do
  destTy <-
    trace ("Looking up type for " ++ pprrString dest) $
    trackUsage Store dest lookupTy
  (t, val') <- checkExpr val
  _ <- unifyTypes Nothing destTy t
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
checkStm For {..}
  --varTy <- lookupTy var
  -- TODO: Bind variable in environment typed depending on ranges of expresions
 = do
  (fromTy, from') <- checkExpr from
    -- TODO: Unless fromTy is int
  (toTy, to') <- checkExpr to
  ty <- unifyTypes (Just (Unsigned (Just 1) noLoc)) fromTy toTy
  -- TODO: Unless toTy is into
  body' <-
    withLocalEnv $ do
      addDefinition var (mkVarDef var ty emptyExt)
      mapM checkStm body
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
checkDef :: (MonadRepr s m) => DefType -> m DefType
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

-- | Generates warnings for every unused variable found in list of definitions
warnUnused :: [DefType] -> TyM ()
warnUnused = mapM_ go
  where
    go VarDef {varState = Unused, varName = name} = tell [UnusedVariable name]
    go ConstDef {constState = Unused, constName = name} =
      tell [UnusedVariable name]
    -- TODO: Warn about unused buses as well
    go _ = pure ()

-- | Runs the type checked on a top-level definition
checkTopDef :: TopDef -> TyM ()
checkTopDef ProcessTable {stms = stms, procName = procName} = do
  updateDefsM_ procName checkDef
  body' <- withScope procName $ do
    res <- mapM checkStm stms
    symTab <- symTable <$> getCurEnv
    warnUnused (M.elems symTab)
    return res
  updateTopDef procName  (\x -> x { stms = body'} )
checkTopDef NetworkTable {..} =
  -- TODO: Check network definitions also.
  updateTopDef netName id

-- | Throws if passed a compound name
ensureSingleRef :: (References a) => a -> Ident
ensureSingleRef r = go $ refOf r
  where
    go (i N.:| []) = i
    go _ =
      throw $
      InternalCompilerError "Compound names should not occur at this point"

reduceRange :: (MonadRepr s m) => Range -> m (Integer, Integer)
reduceRange Range {..} = (,) <$> exprReduceToInt lower <*> exprReduceToInt upper

-- | Create an environment for a process by adding its definitions
buildDeclTab ::
     (MonadRepr s m) => Ident -> [Declaration] -> m (M.HashMap String DefType)
buildDeclTab ctx = foldM go M.empty
  where
    go m (VarDecl v@Variable {..}) = do
      defaultVal <- case val of
        Just e  -> exprReduceToLiteral e
        Nothing -> pure $ LitInt 0 noLoc
      ensureUndef name m $
        return $ M.insert (toString name) (VarDef name v Unused defaultVal Void) m
    go m (ConstDecl c@Constant {..}) = do
      val' <- exprReduceToLiteral val
      ensureUndef name m $
        return $ M.insert (toString name) (ConstDef name c Unused val' Void) m
    go m (BusDecl b@Bus {..}) =
      mkBusDecl m ctx b
    go m (FuncDecl f@Function {..}) =
      ensureUndef name m $
      return $ M.insert (toString name) (FunDef name f Void) m
    go m (EnumDecl e@Enumeration {..})
      -- Assign numbers to unnumbered enum fields in a similar fashion to C. If
      -- the first field f_0 is not assigned a value, it will be given the value
      -- 0, then subsequent fields f_1 will be given the value 1. If a field f_n
      -- is explicitly assigned the value n, the subsequent fields f_{n+1} will
      -- be assigned the value n + 1
     = do
      enumFields <- fillEnum 0 fields
      let maxVal = maximum (map snd enumFields)
      let minVal = minimum (map snd enumFields)
      -- An enum is regular if its first (and minimum) value is 0 and its last
      -- value is the maximum and the distance between every element is exactly
      -- 1.
      -- TODO: Handle single-element enums.
      let isRegular =
            minVal == 0 &&
            all
              (== 1)
              (snd $
               mapAccumR (\x y -> (y, x - y)) maxVal (init $ map snd enumFields))
      res <-
        ensureUndef name m $
        return $
        M.insert
          (toString name)
          (EnumDef name enumFields isRegular (e {ty = typeOf maxVal}) Void)
          m
      -- Insert fields of enum as entries into the entity symbol tab
      foldM
        (\m' (f, v) ->
           ensureUndef f m' $
           return $ M.insert (toString f) (EnumFieldDef f v name Void) m')
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
    go m (InstDecl i) =
      mkInstDecl m i
    go _ GenDecl {} = error "TODO: Generator declarations are unhandeled"

mkInstDecl :: (MonadRepr s m) => SymTab -> Instance -> m SymTab
mkInstDecl m i@Instance {..} =
  let name
        -- Identifiers cannot start with _ so we have those names reserved
        -- for our private namespace. Prefix with __ to avoid clashing with
        -- names introduced by the import handler
        -- TODO: More accurate location of anonymous name
       =
        fromMaybe
          (Ident ("__anonymous_" <> pprr elName) (fromLoc $ locOf elName))
          instName
      isAnon = isNothing instName
  in ensureUndef name m $
     return $
     M.insert
       (toString name)
       (InstDef
          name
          i
          (ensureSingleRef elName)
                   --(map refOf pars)
          isAnon
          Void)
       m

mkBusDecl :: (MonadRepr s m) => SymTab -> Ident -> Bus -> m SymTab
mkBusDecl m ctx b@Bus {..} = do
  shape <- busToShape b
  ensureUndef name m $
    return $
    M.insert
      (toString name)
      (BusDef name (ctx N.:| [name]) shape b Unassigned Shared exposed Void)
      m

buildProcTab :: (MonadRepr s m) => Process -> m TopDef
buildProcTab p@Process {name = n, decls = d, body = body} = do
  tab <- buildDeclTab n d
  return $ ProcessTable tab (nameOf p) [] body M.empty p Void

buildNetTab :: (MonadRepr s m) => Network -> m TopDef
buildNetTab net@Network {name = n, netDecls = d} = do
  tab <- foldM go M.empty d
  return $ NetworkTable tab n [] net Void
  where
    go m (NetConst c@Constant {..}) = do
      val' <- exprReduceToLiteral val
      ensureUndef name m $
        return $ M.insert (toString name) (ConstDef name c Unused val' Void) m
    go m (NetBus b) =
      mkBusDecl m n b
    go m (NetInst i) =
         -- TODO: Maybe we can handle indexed instances by adding a separate
         -- DefType entry IndexedInstDef
      mkInstDecl m i
    go _ NetGen {} = error "TODO: Generator declarations are unhandeled"

-- | Reduce an expression to a name and fail if impossible
exprReduceToName :: (MonadRepr s m) => Expr -> m Name
exprReduceToName PrimName {..} = return name
exprReduceToName e             = throw $ ExprInvalidInContext e

  -- | Reduce an expression to an integer and fail if impossible
exprReduceToInt :: (MonadRepr s m) => Expr -> m Integer
-- TODO: Run expression through simplifier attempting to evaluate it to a
-- constant int
exprReduceToInt (PrimLit _ (LitInt i _) _) = return i
exprReduceToInt e                          = throw $ ExprInvalidInContext e

exprReduceToLiteral :: (MonadRepr s m) => Expr -> m Literal
exprReduceToLiteral (PrimLit _ l _) = return l
exprReduceToLiteral e               = throw $ ExprInvalidInContext e

busToShape :: (MonadRepr s m ) => Bus -> m BusShape
busToShape Bus {signals = signals} = BusShape <$> mapM go signals
  where
    go BusSignal {..} = do
      val <- case value of
        Just e  -> Just <$> exprReduceToLiteral e
        Nothing -> pure Nothing
      return (name, (ty, val))

-- | Group a list of tuples by its first input. Turns [(a, b)] into (a, [b]) for
-- running sequences of identical a's
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
     (MonadRepr s m, Located l)
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

-- TODO: Set the param attribute of instances to something reasonable.

-- | Infer the types of process parameters by checking how they are instantiated
-- and normalize instance parameter list to a <name>: <value> format
inferParamTypes ::
     (MonadRepr Void m)
  => [(Ident, Instance)]
  -> m [(Ident, [(Ident, ParamType)], Instance)]
inferParamTypes insts = do
  instTypes <-
    forM
      insts
      (\(instCtxName, inst) -> (withScope instCtxName . go instCtxName) inst)
  params <- groupWithM unifyProcParam $ sortOn fst instTypes
  return $ map (\(i, (newInst, pars)) -> (i, pars, newInst)) params
  where
    go ctx i@Instance {elName = instantiated, params = actual} = do
      el <- lookupTopDef instantiated
      let formal = getParams el
          elName = nameOf el
      -- Add entity to the set of used entities
      -- TODO: This is a bit of hack. Unused entities should be filtered away by
      -- the set of used entities.
      addUsedEnt el
      -- Also add instantiating entity
      addUsedEnt ctx
      unless
        (length formal == length actual)
        (throw $ ParamCountMismatch (length actual) (length formal) i elName)
      -- Generate list of inferred parameter types for this instance
      --(elName, ) . (i, ) <$>
      res <- zipWithM
          (\Param {count = count, dir = forDir, name = forName} (ident, e)
            -- If name is unnamed, substitute matching name to normalize
            -- parameter listings. If name is specified, make sure that the name
            -- used in the actual parameter matches the name of the formal
            -- parameter
            -> do
             let actualName = fromMaybe forName ident
             unless (forName == actualName) $
               throw $ NamedParameterMismatch forName actualName elName
              -- For every parameter, return a name, type tuple.
             case forDir of
               Const _ -> do
                 -- TODO: We need to make sure that e refers to only constant
                 -- values here (i.e., only constants and cont parameters)
                 -- Restrictions like this could be placed in a reader monad
                 (_, eTy) <- checkExpr e
                 pure (forName, ConstPar (typeOf eTy))
               In _ -> mkBusDef Input e forName count instantiated
               Out _ -> mkBusDef Output e forName count instantiated
          )
          formal
          actual
      let newInstPars = zipWith (\(n, _) (_, e) -> (Just n, e)) res actual
          newInst = i { params = newInstPars } :: Instance
      return (elName, (newInst, res))
    mkBusDef dir e forName count instantiated = do
      count' <-
        case count of
          Just (Just expr) -> exprReduceToInt expr >>= pure . Just
          _                -> pure Nothing
      locRef <- exprReduceToName e
      bDef <- lookupBus locRef
      withScope instantiated $ setUsedBus (busRef bDef) (refOf forName, dir)
      shape <- busToShape (busDef bDef)
      return
        ( forName
        , BusPar
          { ref = busRef bDef
          , localRef = refOf locRef
          , busShape = shape
          , busState = dir
          , array = count'
          })
                                -- bus here. Find some way of identifying buses
                                -- by matching their shapes/field names
    getParams ProcessTable {procDef = procDef} =
      (params :: Process -> [Param]) procDef
    getParams NetworkTable {netDef = netDef} =
      (params :: Network -> [Param]) netDef

-- | Populate typechecking environment with all functions defined.
buildEnv :: DesignFile -> TyM ()
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
               NetworkTable {..} ->
                 (netName, ) <$> (universeBi netDef :: [Instance])
               ProcessTable {..} ->
                 (procName, ) <$> (universeBi procDef :: [Instance]))
          topDefs
  params <- inferParamTypes insts
  trace (ppShow params) $
    forM_
      params
      -- TODO: What to do about newInst here
      (\(i, t, _newInst) -> do
         updateTopDef
           i
           (\x ->
              case x of
                nt@NetworkTable {} -> nt {params = t} :: TopDef
                pc@ProcessTable {} -> pc {params = t} :: TopDef)
         forM_
           t
           (\(n, param) ->
              trace
                ("Added definition " ++
                 show i ++ " " ++ show n ++ " " ++ show param)
                addDefinition'
                i
                n
                (ParamDef n param Void)))
  mapUsedTopDefsM_ checkTopDef

-- | Do typechecking of an environment. Return DesignFile with completed type
-- annoations
--typeCheck :: (MonadIO m) => DesignFile -> m DesignFile
-- typeCheck :: DesignFile -> DesignFile
typeCheck :: DesignFile -> IO Env
typeCheck df = do
  let (_, env) = runReprMidentity (mkEnv Void) (runWriterT $ unTyM (buildEnv df))
  return env
