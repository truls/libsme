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

import           Control.Exception.Safe
import           Control.Monad               (foldM, forM, forM_, unless,
                                              zipWithM)
import           Control.Monad.Identity      (runIdentity)
import           Control.Monad.Reader        (MonadReader, ReaderT, asks, local,
                                              runReaderT)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Writer        (MonadWriter, WriterT, runWriterT,
                                              tell)
import           Data.Generics.Uniplate.Data (universeBi)
import qualified Data.HashMap.Strict         as M
import           Data.List                   (mapAccumR, sortOn)
import qualified Data.List.NonEmpty          as N
import           Data.Loc                    (Located (..), fromLoc, noLoc)
import           Data.Maybe                  (fromMaybe, isNothing)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.Error
import           SME.Representation
import           SME.Warning

--import           Text.Show.Pretty            (ppShow)

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

-- | Main typechecking monad
newtype TyM a = TyM
  { unTyM :: ReaderT Context (WriterT Warns (ReprM (Either SomeException) Void)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Context
             , MonadWriter Warns
             , MonadState Env
             --, MonadError TypeCheckErrors
             , MonadThrow
             )

instance (MonadRepr Void) TyM
--instance (MonadThrow) TyM

data AccessType
  = Store
  | Load
  deriving Eq

data Context = Context
  { inLoop :: Bool
  }

mkContext :: Context
mkContext = Context { inLoop = False }

withInLoop :: TyM a -> TyM a
withInLoop = local (\x -> x { inLoop = True })

isInLoop :: TyM Bool
isInLoop = asks inLoop

lookupBus :: (References a, MonadRepr Void m) => a -> m DefType
lookupBus r =
  let ref = trace ("lookupBus called with " ++ show (refOf r)) (refOf r)
  in lookupDef r >>= \case
       b@BusDef {} -> pure b
       _ -> throw $ ExpectedBus (N.head ref)

-- hasArrayAccess :: Name -> Bool
-- hasArrayAccess Name {..} =
--   flip any (N.toList parts) isArray

-- isArray :: NamePart -> Bool
-- isArray  IdentName {}  = False
-- isArray ArrayAccess {} = True

-- TODO: Handle multi-dimensional arrays
-- This is a bit of hack. The idea is that if the final component of a name is
-- an array, the name is an array lookup and we need to return the element type
-- of the array. If any other component of the name is an array lookup, then we
-- can discard the array indexing from a type point-of-view since all elements
-- has the same type.
lookupName :: (MonadRepr s m) => Name -> m Typeness
lookupName n@Name {parts = parts} = case N.last parts of
  ArrayAccess {} ->
    lookupTy n >>= \case
      -- TODO: Location of innerTy is probably not set
      Typed Array {..} -> return $ Typed innerTy
      _ -> throw $ NotAnArray n
  IdentName {} -> lookupTy n

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
 = do
  lookupDef r >>= \case
    ConstDef {} -> do
      unless (t == Load) $ throw $ WroteConstant r
      updateDef r (\x -> Just x {constState = Used})
    VarDef {} ->
      updateDef r (\x -> Just x {varState = Used})
    BusDef {busRef = bRef} -> do
      bus <- lookupDef bRef
      getBusState bRef >>= \case
        Nothing
          -- At this point we should only meet locally declared buses as all
          -- has been added by the parameter type inference function.
         -> do
          direct <- isTopDef r
          setUsedBus
            bRef
            -- TODO: Account for the case where we reference a bus declared in
            -- another process by the name of the process.
            ( refOf $ nameOf bus
            , if t == Load
                then Input
                else Output
            , direct)
        Just a ->
          forM_
            (map (t, ) a)
            (\case
               (Load, (_, Input, _)) -> return ()
               (Load, _) -> throw $ ReadOutputBus r
               (Store, (_, Output, _)) -> return ()
               (Store, _) -> throw $ WroteInputBus r)
    _ -> return ()
  act r


-- | Return true if type 'b' can be "contained" by type 'a'
typeSubsetOf :: (MonadRepr s m) => Typeness -> Typeness -> m ()
typeSubsetOf t1@(Typed a) t2@(Typed b)
  -- TODO: Make a dedicated error for this function.
 = unless (go a b) $ throw $ TypeMismatchError t2 t1
  where
    go (Unsigned Nothing _) (Unsigned _ _)         = True
    go (Signed Nothing _) (Unsigned _ _)           = True
    go (Signed Nothing _) (Signed _ _)             = True
    go (Unsigned l1 _) (Unsigned l2 _)             = l1 >= l2
    go (Signed (Just l1) _) (Unsigned (Just l2) _) = l1 >= (l2 + 1)
    go (Unsigned (Just l1) _) (Signed (Just l2) _) = l1 >= (l2 + 1)
    go (Signed l1 _) (Signed l2 _)                 = l1 >= l2
    go Array {innerTy = t1'} Array {innerTy = t2'} = go t1' t2'
    go l1 l2                                       = l1 == l2
-- FIXME: This case was added for undef literals but there is probably a better
-- way of handling them. Maybe add a dedicated type
typeSubsetOf _ Untyped = return ()
typeSubsetOf t1 t2 = trace "Subset of" throw $ TypeMismatchError t2 t1

ensureTypeSubsetOf :: (MonadRepr s m) => Typeness -> m ()
ensureTypeSubsetOf t1 =
  getTypeCtx >>= \case
    Just t -> typeSubsetOf t t1
    Nothing -> return ()

unifyTypes ::
     (MonadRepr s m) => Maybe Type -> Typeness -> Typeness -> m Typeness
unifyTypes expected t1 t2 = do
  traceM $ "Checking " ++ show t1 ++ " " ++ show t2
  res <- go t1 t2
  ensureTypeSubsetOf res
  case expected of
    Just t  -> unifyTypes Nothing (Typed t) res
      -- unless (Typed t == res) $ throw $ TypeMismatchError (Typed t) res
      -- return (Typed t)
    Nothing -> return res
  where
    go Untyped Untyped         = error "Untyped" -- TODO: Better error
    go Untyped t               = pure t
    go t Untyped               = pure t
    go (Typed t1') (Typed t2') = Typed <$> go' t1' t2'
    go' (Unsigned (Just l1) loc) (Unsigned (Just l2) _) =
      return $ Unsigned (Just (max l1 l2)) loc
    go' (Signed (Just l1) loc) (Signed (Just l2) _) =
      return $ Signed (Just (max l1 l2)) loc
    go' (Unsigned (Just l1) loc) (Signed (Just l2) _)
     =
      trace ("Unsigned " ++ show l1 ++ " -> Signed " ++ show l2) $
      return $
      Signed
        (Just
           (max l1 l2 +
            (if l2 >= l1
               then 1
               else 0)))
        loc
    go' (Signed (Just l1) loc) (Unsigned (Just l2) _) =
      trace "Signed -> Unsigned" return $
      Signed
        (Just
           (max l1 l2 +
            (if l2 >= l1
               then 1
               else 0)))
        loc
    go' (Unsigned Nothing loc) (Unsigned (Just s) _) =
      return (Unsigned (Just s) loc)
    go' (Unsigned (Just s) loc) (Unsigned Nothing _) =
      return (Unsigned (Just s) loc)
    go' (Signed Nothing loc) (Unsigned (Just s) _) =
      return (Signed (Just s) loc)
    go' (Signed (Just s) loc) (Unsigned Nothing _) =
      return (Signed (Just s) loc)
    go' (Signed Nothing loc) (Signed (Just s) _) = return (Signed (Just s) loc)
    go' (Signed (Just s) loc) (Signed Nothing _) = return (Signed (Just s) loc)
    go' (Unsigned Nothing loc) (Signed (Just s) _) =
      return (Signed (Just s) loc)
    go' (Unsigned (Just s) loc) (Signed Nothing _) =
      return (Signed (Just s) loc)
    go' (Unsigned Nothing loc) (Signed Nothing _) = return (Signed Nothing loc)
    go' (Signed Nothing loc) (Unsigned Nothing _) = return (Signed Nothing loc)
    go' (Signed Nothing loc) (Signed Nothing _) = return (Signed Nothing loc)
    go' (Unsigned Nothing loc) (Unsigned Nothing _) =
      return (Unsigned Nothing loc)
    go' t1'@(Array l1 ity1 _loc) t2'@(Array l2 ity2 loc) =
      unifyTypes Nothing (Typed ity1) (Typed ity2) >>= \case
        (Typed ity) -> do
          unless (l1 == l2) $ throw $ TypeMismatchError (Typed t1') (Typed t2')
          return (Array l1 ity loc)
        Untyped -> throw $ TypeMismatchError (Typed t2') (Typed t1')
    go' (Bool loc) (Bool _) = return (Bool loc)
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

-- | Given a binary operator and a type. Returns the type returned by the
-- operand and raises an exception if operator is invalid for type
retTypeBin :: (MonadRepr s m) => BinOp -> Typeness -> m Typeness
retTypeBin _ Untyped = pure Untyped
retTypeBin o (Typed t) = Typed <$> go o t
  where
    go PlusOp {} Signed {}    = pure t
    go PlusOp {} Unsigned {}  = pure t
    go PlusOp {} Single {}    = pure t
    go PlusOp {} Double {}    = pure t
    go MinusOp {} Signed {}   = pure t
    go MinusOp {} Unsigned {} = pure t
    go MinusOp {} Single {}   = pure t
    go MinusOp {} Double {}   = pure t
    go MulOp {} Signed {}     = pure t
    go MulOp {} Unsigned {}   = pure t
    go MulOp {} Single {}     = pure t
    go MulOp {} Double {}     = pure t

    go DivOp {} Signed {}     = pure t
    go DivOp {} Unsigned {}   = pure t
    go DivOp {} Single {}     = pure t
    go DivOp {} Double {}     = pure t

    go ModOp {} Signed {}     = pure t
    go ModOp {} Unsigned {}   = pure t
    go SllOp {} Signed {}     = pure t
    go SllOp {} Unsigned {}   = pure t
    go SrlOp {} Signed {}     = pure t
    go SrlOp {} Unsigned {}   = pure t
    go AndOp {} Signed {}     = pure t
    go AndOp {} Unsigned {}   = pure t
    go OrOp {} Signed {}      = pure t
    go OrOp {} Unsigned {}    = pure t
    go XorOp {} Signed {}     = pure t
    go XorOp {} Unsigned {}   = pure t

    go ConOp {} Bool {}       = pure t
    go DisOp {} Bool {}       = pure t

    go EqOp {} Signed {..}    = pure (Bool loc)
    go EqOp {} Unsigned {..}  = pure (Bool loc)
    go EqOp {} Single {..}    = pure (Bool loc)
    go EqOp {} Double {..}    = pure (Bool loc)
    go EqOp {} Bool {..}      = pure (Bool loc)

    go NeqOp {} Signed {..}   = pure (Bool loc)
    go NeqOp {} Unsigned {..} = pure (Bool loc)
    go NeqOp {} Single {..}   = pure (Bool loc)
    go NeqOp {} Double {..}   = pure (Bool loc)
    go NeqOp {} Bool {..}     = pure (Bool loc)

    go GeqOp {} Signed {..}   = pure (Bool loc)
    go GeqOp {} Unsigned {..} = pure (Bool loc)
    go GeqOp {} Single {..}   = pure (Bool loc)
    go GeqOp {} Double {..}   = pure (Bool loc)

    go LeqOp {} Signed {..}   = pure (Bool loc)
    go LeqOp {} Unsigned {..} = pure (Bool loc)
    go LeqOp {} Single {..}   = pure (Bool loc)
    go LeqOp {} Double {..}   = pure (Bool loc)

    go GtOp {} Signed {..}    = pure (Bool loc)
    go GtOp {} Unsigned {..}  = pure (Bool loc)
    go GtOp {} Single {..}    = pure (Bool loc)
    go GtOp {} Double {..}    = pure (Bool loc)

    go LtOp {} Signed {..}    = pure (Bool loc)
    go LtOp {} Unsigned {..}  = pure (Bool loc)
    go LtOp {} Single {..}    = pure (Bool loc)
    go LtOp {} Double {..}    = pure (Bool loc)
    go op ty                  = throw $ OperandType op ty

retTypeUn :: (MonadRepr s m) => UnOp -> Typeness -> m Typeness
retTypeUn _ Untyped = pure Untyped
retTypeUn o (Typed t) = Typed <$> go o t
  where
    go UnPlus {} Signed {}    = pure t
    go UnPlus {} Unsigned {}  = pure t
    go UnPlus {} Single {}    = pure t
    go UnPlus {} Double {}    = pure t
    go UnMinus {} Signed {}   = pure t
    go UnMinus {} Unsigned {} = pure t
    go UnMinus {} Single {}   = pure t
    go UnMinus {} Double {}   = pure t
    go NotOp {} Bool {}       = pure t
    go NegOp {} Signed {}     = pure t
    go NegOp {} Unsigned {}   = pure t
    go op ty                  = throw $ OperandType op ty


checkExpr' :: (MonadRepr s m) => Expr -> m Expr
checkExpr' e = snd <$> checkExpr e

-- | Check expression and update references as needed
checkExpr :: (MonadRepr s m) => Expr -> m (Typeness, Expr)
checkExpr p@PrimName {..} = do
  ty' <- trackUsage Load name lookupName
  return (ty', p {ty = ty'} :: Expr)
checkExpr p@FunCall {..}  = (,p) <$> lookupName name
checkExpr p@PrimLit {..} = do
  let t = typeOf lit
  ensureTypeSubsetOf t
  return (t, p {ty = t} :: Expr)
checkExpr Binary {..} = do
  -- TODO: Check that types are supported for operands
  (t1, e1) <- checkExpr left
  (t2, e2) <- checkExpr right
  t' <- unifyTypes Nothing t1 t2
  retTy <- retTypeBin binOp t'
  return (retTy, Binary retTy binOp e1 e2 loc)
checkExpr Unary {..} = do
  (t', e') <- checkExpr right
  t'' <- retTypeUn unOp t'
  let t''' =
        case unOp of
          UnMinus _ -> flipSign t''
          _         -> t'
  return (t''', Unary t''' unOp e' loc)
checkExpr Parens {..} = do
  (t', e') <- checkExpr innerExpr
  return (t', Parens t' e' loc)

-- | Check statements and update references as needed
checkStm :: Statement -> TyM Statement
checkStm Assign {..} = do
  destTy <- trackUsage Store dest lookupName
  -- TODO: Clean up these double withTypectx
  (_t, val') <- withTypeCtx destTy $ checkExpr val
  --_ <- withTypeCtx destTy $ unifyTypes Nothing destTy t
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
 = do
  (fromTy, from') <- checkExpr from
    -- TODO: Unless fromTy is int
  (toTy, to') <- checkExpr to
  ty <- unifyTypes (Just (Unsigned (Just 1) noLoc)) fromTy toTy
  -- TODO: Unless toTy is into
  body' <-
    withLocalEnv $ withInLoop $ do
      addDefinition var (mkVarDef var ty emptyExt)
      mapM checkStm body
  return $ For var from' to' body' loc
checkStm Switch {..} = do
  value' <- checkExpr' value
  cases' <-
    mapM
      (\(e, stms) -> do
         e' <- checkExpr' e
         stms' <- mapM checkStm stms
         return (e', stms'))
      cases
  -- TODO: Make sure that the types of all the cases checks out and matches type
  -- of value
  defaultCase' <-
    case defaultCase of
      Just stms -> Just <$> mapM checkStm stms
      Nothing   -> pure Nothing
  return $ Switch value' cases' defaultCase' loc
checkStm tr@Trace {..} =
  case str of
    LitString {stringVal = stringVal} -> do
      let len = T.count "{}" stringVal
      unless (len == length subs) $
        throw $ FormatStringMismatch len (length subs) tr
      subs' <- mapM checkExpr' subs
      return $ Trace str subs' loc
    _ ->
      throw $
      ArgumentError tr "First argument of trace must be a string literal"
checkStm as@Assert {..}
 = do
  -- If the first argument is present, make sure that it is a string
  case descr of
    Nothing -> return ()
    Just LitString {} -> return ()
    _ ->
      throw $
      ArgumentError
        as
        "First argument of assert must be a string literal or condition"
  (ty', c') <- checkExpr cond
  -- TODO: Replace this with an explicit expectation parameter for checkExpr
  _ <- unifyTypes Nothing ty' (Typed (Bool noLoc))
  return $ Assert descr c' loc

checkStm Barrier {..} = return $ Barrier loc
checkStm br@Break {..} =
  isInLoop >>= \case
    False -> throw $ BreakOutsideLoop br
    True -> return $ Break loc
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
checkDef :: DefType -> TyM DefType
checkDef v@VarDef {..}
  -- Check that range is within type constraint
 = do
  res <- withTypeCtx (typeOf varDef) $ go varDef
  val' <- case varVal of
    Just dv -> Just <$> getConstVal dv
    Nothing -> pure Nothing
  return $ v {varDef = res, varVal = val'}
    -- TODO: Actually check that a) default value, if existing, is within type
    -- and range constraints and that range constraints, if existing, is within
    -- type bounds
  where
    go Variable {..} = do
      e' <-
        case val of
          Just e -> do
            ee <- withTypeCtx ty $ checkExpr' e
            pure (Just ee)
          Nothing -> pure val
      return $ Variable name ty e' range loc
checkDef c@ConstDef {..} = do
  res <- go constDef
  val' <- getConstVal constVal
  return $ c {constDef = res, constVal = val'}
  where
    go ci@Constant {..} = do
      e' <- withTypeCtx ty $ checkExpr' val
      -- FIXME: This will emit a warning for non-int types.
      unless (isUnsized ty) $ tell [BoundedVarForConst ci ty]
      return $ Constant name ty e' loc
checkDef b@BusDef {busDef = busDef, busShape = busShape} = do
  res <- go busDef
  bs' <- BusShape <$> mapM checkBS (unBusShape busShape)
  return $ b {busDef = res, busShape = bs'}
  where
    checkBS (i, (ty, e, r)) = do
      e' <-
        case e of
          Just v  -> Just <$> getConstVal v
          Nothing -> pure Nothing
      return (i, (ty, e', r))
    go bd@Bus {signals = signals} = do
      signals' <-
        forM
          signals
          (\bsig@BusSignal {..}
             -- TODO: Check bus signal declared type against default value and
             -- range
            -> do
             value' <-
               case value of
                 Just v  -> Just <$> getConstVal v
                 Nothing -> pure Nothing
             return (bsig {value = value'} :: BusSignal))
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
  withScope procName $ updateDefsM_ procName checkDef
  body' <- withScope procName $ do
    res <- mapM checkStm stms
    symTab <- symTable <$> getCurEnv
    warnUnused (M.elems symTab)
    return res
  updateTopDef procName  (\x -> x { stms = body'} )
checkTopDef NetworkTable {netName = netName} = do
  withScope netName $ updateDefsM_ netName checkDef
  updateTopDef netName id
  withScope netName $ do
    symTab <- symTable <$> getCurEnv
    warnUnused (M.elems symTab)

-- | Throws if passed a compound name
ensureSingleRef :: (MonadThrow m, References a) => a -> m Ident
ensureSingleRef r = go $ refOf r
  where
    go (i N.:| []) = pure i
    go _           = bad "Compound names should not occur at this point"

-- reduceRange :: (MonadRepr s m) => Range -> m (Integer, Integer)
-- reduceRange Range {..} = (,) <$> exprReduceToInt lower <*> exprReduceToInt upper

-- | Create an environment for a process by adding its definitions
buildDeclTab ::
     (MonadRepr s m) => Ident -> [Declaration] -> m (M.HashMap String DefType)
buildDeclTab ctx = foldM go M.empty
  where
    go m (VarDecl v@Variable {..}) = do
      defaultVal <-
        case val of
          Just v' -> Just <$> exprReduceToSimpleExpr v'
          Nothing -> pure Nothing
      ensureUndef name m $
        return $
        M.insert (toString name) (VarDef name v Unused defaultVal Void) m
    go m (ConstDecl c@Constant {..}) = do
      val' <- exprReduceToSimpleExpr val
      ensureUndef name m $
        return $ M.insert (toString name) (ConstDef name c Unused val' Void) m
    go m (BusDecl b@Bus {..}) = mkBusDecl m ctx b
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
      enumFields <- fillEnum 0 (N.toList fields)
      let maxVal = maximum (map snd enumFields)
      let minVal = minimum (map snd enumFields)
      -- An enum is regular if its first (and minimum) value is 0 and its last
      -- value is the maximum and the distance between every element is exactly
      let isRegular =
            if length fields == 1
              then minVal == 0
              else minVal == 0 &&
                   all
                     (== 1)
                     (snd $
                      mapAccumR
                        (\x y -> (y, x - y))
                        maxVal
                        (init $ map snd enumFields))
      res <-
        ensureUndef name m $
        return $
        M.insert
          (toString name)
          (EnumDef name enumFields isRegular (e {ty = typeOf maxVal}) Void)
          m
           -- Insert fields of enum as entries into the entity symbol tab
      foldM
        (\m' (field, v) ->
           ensureUndef field m' $
           return $
           M.insert (toString field) (EnumFieldDef field v name Void) m')
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
    go m (InstDecl i) = mkInstDecl m i
    go _ GenDecl {} = error "TODO: Generator declarations are unhandeled"

-- Identifiers cannot start with _ so we have those names reserved
-- for our private namespace. Prefix with __ to avoid clashing with
-- names introduced by the import handler
  -- TODO: More accurate location of anonymous name
mkAnonName :: (Pretty p, Located p) => Maybe Ident -> p -> Ident
mkAnonName i n =
  fromMaybe (Ident ("__anonymous_" <> pprr n) (fromLoc $ locOf n)) i

mkInstDecl :: (MonadRepr s m) => SymTab -> Instance -> m SymTab
mkInstDecl m i@Instance {..} =
  let name = mkAnonName instName elName
      isAnon = isNothing instName
  in do ref' <- ensureSingleRef elName
        ensureUndef name m $
          return $
          M.insert (toString name) (InstDef name i ref' [] isAnon Void) m

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
  return $ NetworkTable tab n [] net False Void
  where
    go m (NetConst c@Constant {..}) =
      ensureUndef name m $
        return $ M.insert (toString name) (ConstDef name c Unused val Void) m
    go m (NetBus b) = mkBusDecl m n b
    go m (NetInst i)
         -- TODO: Maybe we can handle indexed instances by adding a separate
         -- DefType entry IndexedInstDef
     = mkInstDecl m i
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

exprReduceToSimpleExpr :: (MonadRepr s m) => Expr -> m Expr
exprReduceToSimpleExpr e@(PrimLit t _ loc) = do
  l' <- exprReduceToLiteral e
  return (PrimLit t l' loc)
exprReduceToSimpleExpr e@PrimName {} = return e
exprReduceToSimpleExpr e = throw $ ExprInvalidInContext e

-- TODO: This is a hack
getConstVal :: (MonadRepr s m) => Expr -> m Expr
getConstVal p@PrimLit {} = return p
getConstVal PrimName {name = n@Name {..}} =
  trackUsage Load n lookupDef >>= \case
    ConstDef {..} -> return constVal
    _ -> error "Only constants may be used as initializers"
getConstVal _ = error "Only constants or literals may be used as initializers"

busToShape :: (MonadRepr s m ) => Bus -> m BusShape
busToShape Bus {signals = signals} = BusShape <$> mapM go signals
  where
    go BusSignal {..} = do
      val <-
        case value of
          Just e  -> Just <$> exprReduceToSimpleExpr e
          Nothing -> pure Nothing
      range' <-
        case range of
          Just (Range l u _) -> do
            l' <- exprReduceToLiteral l
            u' <- exprReduceToLiteral u
            return $ Just (l', u')
          Nothing -> pure Nothing
      return (name, (ty, val, range'))

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

  -- | Non monadic version of eq-sort
groupWith :: (Eq a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
groupWith f i =
  let f' a b = pure $ f a b
  in runIdentity $ groupWithM f' i

-- | Unifies the list of parameters originating from a single entity.
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
      --trace ("Unified ( " ++ show i1 ++ " types " ++ show t1 ++ " and " ++
      --show t2 ++ " yielding " ++ show res) $
      return (i1, ConstPar res)
    go (i1, t1@BusPar {parBusShape = bs1}) (_, BusPar {parBusShape = bs2})
      --trace ("Comparing " ++ show t1 ++ " and " ++ show t2) $
     =
      unless (bs1 == bs2) (throw $ BusShapeMismatch bs1 bs2 inst) >>
      return (i1, t1)
    go _ _ = throw $ InstanceParamTypeMismatch inst

-- TODO: Set the param attribute of instances to something reasonable.

fixupNewInstDecls :: (Ord a, Ord b) => [(a, b, c)] -> [(a, b, [c])]
fixupNewInstDecls =
  map (\((a, b), c) -> (a, b, c)) .
  groupWith (<>) . sortOn fst . map (\(a, b, c) -> ((a, b), [c]))


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
  let (elNames, newInsts, ress) = unzip3 instTypes
      (params, newInstDecls) = unzip $ map unzip ress
      params' = zip elNames (zip newInsts params)
  params'' <- groupWithM unifyProcParam $ sortOn fst params'
  mapM_
    (\(x, y, z) -> updateInstParam x y z)
    (concatMap fixupNewInstDecls newInstDecls)
  return $ map (\(i, (newInst, pars)) -> (i, pars, newInst)) params''
  where
    go ctx i@Instance { elName = instantiated
                      , params = actual
                      , instName = instName
                      } = do
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
      res <-
        zipWithM
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
               Const _
                 -- TODO: We need to make sure that e refers to only constant
                 -- values here (i.e., only constants and cont parameters)
                 -- Restrictions like this could be placed in a reader monad
                -> do
                 eTy <- checkExpr' e
                 pure
                   ( (forName, ConstPar (typeOf eTy))
                   , (ctx, mkAnonName instName instantiated, InstConstPar eTy))
               In _ -> mkBusDef Input e forName count instantiated instName ctx
               Out _ ->
                 mkBusDef Output e forName count instantiated instName ctx)
          formal
          actual
      let newInstPars = zipWith (\((n, _), _) (_, e) -> (Just n, e)) res actual
          newInst = i {params = newInstPars} :: Instance
      return (elName, newInst, res)
    mkBusDef dir e forName count instantiated instName ctx = do
      count' <-
        case count of
          Just (Just expr) -> exprReduceToInt expr >>= pure . Just
          _                -> pure Nothing
      locRef <- exprReduceToName e
      bDef <- lookupBus locRef
      withScope instantiated $
        setUsedBus (busRef bDef) (refOf forName, dir, False)
      shape <- busToShape (busDef bDef)
      return
        ( ( forName
          , BusPar
            { ref = busRef bDef
            , localRef = refOf forName
            , parBusShape = shape
            , busState = dir
            , array = count'
            })
        , (ctx, mkAnonName instName instantiated, InstBusPar (refOf locRef)))
          -- bus here. Find some way of identifying buses
          -- by matching their shapes/field names
    getParams ProcessTable {procDef = procDef} =
      (params :: Process -> [Param]) procDef
    getParams NetworkTable {netDef = netDef} =
      (params :: Network -> [Param]) netDef

-- | Adds params 'pars' in instance 'd' located in entity 'i'
updateInstParam :: (MonadRepr Void m) => Ident -> Ident -> [InstParam] -> m ()
updateInstParam i d pars =
  withScope i $
  updateDef
    d
    (\case
       idef@InstDef {} -> Just (idef {params = pars} :: DefType)
       _ -> Nothing)

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
typeCheck :: (MonadIO m, MonadThrow m) => DesignFile -> Config -> m (Env, Warns)
typeCheck df conf = do
  let res =
        runReprM
          (mkEnv conf Void)
          (runWriterT $ runReaderT (unTyM (buildEnv df)) mkContext)
  case res of
    Left e            -> throw e
    Right ((_, w), r) -> return (r, w)
