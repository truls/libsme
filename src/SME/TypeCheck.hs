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
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Writer        (MonadWriter, WriterT, runWriterT)
import           Data.Generics.Uniplate.Data (universeBi)
import qualified Data.HashMap.Strict         as M
import           Data.List                   (sortOn)
import qualified Data.List.NonEmpty          as N
import           Data.Loc                    (Loc, Located (..), SrcLoc (..),
                                              noLoc)
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Monoid                 ((<>))

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.Error
import           SME.Representation


import           Text.Show.Pretty            (ppShow)

--import           Debug.Trace                 (trace)
trace :: String -> a -> a
trace _ = id

-- * Type checking monad and data structures for holding type checking state

type Env = BaseEnv Void
type DefType = BaseDefType Void
type TopDef = BaseTopDef Void

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
  { unTyM :: WriterT Log (ReprM Identity Void) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadWriter Log
             , MonadState Env
             , MonadError TypeCheckErrors
             )

instance (MonadRepr Void) TyM

lookupBus :: (References a, MonadRepr Void m) => a -> m DefType
lookupBus r =
  let ref = trace ("lookupBus called with " ++ show (refOf r)) (refOf r)
  in lookupDef r >>= \case
       b@BusDef {} -> pure b
       _ -> throw $ ExpectedBus (N.head ref)

lookupTy ::
     (MonadRepr s m, References a, Located a, Pretty a) => a -> m Typeness
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
     (MonadRepr s m) => Maybe Type -> Typeness -> Typeness -> m Typeness
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
checkExpr :: (MonadRepr s m) => Expr -> m (Typeness, Expr)
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
checkStm :: (MonadRepr s m) => Statement -> m Statement
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



checkTopDef :: (MonadRepr Void m) => TopDef -> m ()
checkTopDef ProcessTable { ..} = do
  mapDefs procName checkDef
  body' <- withScope procName $ mapM checkStm stms
  updateTopDef procName  (\x -> x { stms = body'} )
checkTopDef NetworkTable {..} =
  updateTopDef netName id
checkTopDef EmptyTable = return ()

ensureSingleRef :: (References a) => a -> Ident
ensureSingleRef r = go $ refOf r
  where
    go (i N.:| []) = i
    go _ =
      throw $
      InternalCompilerError "Compound names should not occur at this point"

-- | Create an environment for a process by adding its definitions
buildDeclTab :: (MonadRepr s m) => Ident -> [Declaration] -> m (M.HashMap String DefType)
buildDeclTab ctx = foldM go M.empty
  where
    go m (VarDecl v@Variable {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (VarDef name v Void) m
    go m (ConstDecl c@Constant {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (ConstDef name c Void) m
    go m (BusDecl b@Bus {..}) =
      ensureUndef name m $
      return $
      M.insert (toString name) (BusDef name [ctx, name] (busToShape b) b Void) m
    go m (FuncDecl f@Function {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (FunDef name f Void) m
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
          (EnumDef name enumFields (e {ty = typeOf maxVal}) Void)
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
    go m (InstDecl i@Instance {..}) =
      let name =
            fromMaybe
            -- Identifiers cannot start with _ so everything starting with _ is
            -- reserved for our private namespace
              (Ident ("_anonymous_" <> toText (nameOf i)) noLoc)
              instName
          isAnon = isNothing instName
      in ensureUndef name m $
         return $
         M.insert
           (toString name)
           (InstDef name i (ensureSingleRef elName) [] isAnon Void)
           m
    go _ GenDecl {} = error "TODO: Generator declarations are unhandeled"

buildProcTab :: (MonadRepr s m) => Process -> m TopDef
buildProcTab p@Process {name = n, decls = d, body = body} = do
  tab <- buildDeclTab n d
  return $ ProcessTable tab (nameOf p) M.empty body p Void

buildNetTab :: (MonadRepr s m) => Network -> m TopDef
buildNetTab net@Network {name = n, netDecls = d} = do
  tab <- foldM go M.empty d
  return $ NetworkTable tab n M.empty net Void
  where
    go m (NetConst c@Constant {..}) =
      ensureUndef name m $ return $ M.insert (toString name) (ConstDef name c Void) m
    go m (NetBus b@Bus {..}) =
      ensureUndef name m $
      return $
      M.insert (toString name) (BusDef name [n, name] (busToShape b) b Void) m
    go m (NetInst i@Instance {..}) =
      let name
            -- Identifiers cannot start with _ so we have those names reserved
            -- for our private namespace. Prefix with __ to avoid clashing with
            -- names introduced by the import handler
           =
            fromMaybe
              (Ident ("__anonymous_" <> pprr elName) noLoc)
              instName
          isAnon = isNothing instName
      in ensureUndef name m $
         return $
         M.insert
           (toString name)
           (InstDef name i (ensureSingleRef elName) [] isAnon Void)
           m
         -- TODO: Maybe we can handle indexed instances by adding a separate
         -- DefType entry IndexedInstDef
    go _ NetGen {} = error "TODO: Generator declarations are unhandeled"

-- | Reduce an expression to a name and fail if impossible
exprReduceToName :: (MonadRepr s m) => Expr -> m Name
exprReduceToName PrimName {..} = return name
exprReduceToName e             = throw $ ExprInvalidInContext e

exprReduceToInt :: (MonadRepr s m) => Expr -> m Integer
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

-- | Infer the types of process parameters by checking how they are instantiated
-- and normalize instance parameter list to a <name>: <value> format
inferParamTypes ::
     (MonadRepr Void m)
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
                (ParamDef n param Void)))
  forUsedTopDefs checkTopDef
  where
    toMap els = M.fromList (map (first toString) els)

-- | Do typechecking of an environment. Return DesignFile with completed type
-- annoations
--typeCheck :: (MonadIO m) => DesignFile -> m DesignFile
-- typeCheck :: DesignFile -> DesignFile
typeCheck :: DesignFile -> IO DesignFile
typeCheck df = do
  let res = runWriterT $ unTyM (buildEnv df)
      res' = runReprMidentity (mkEnv Void) res
  liftIO $ putStrLn $ ppShow res'
  return df
  -- liftIO $ putStrLn $ ppShow e
  -- liftIO $ putStrLn $ ppShow w
