{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

-- | SME network simulator

module SME.Simulate
  ( simulate
  , SimEnv
  , SimM(..)
  , newSteppingSim
  , procStep
  , busStep
  , applyTypes
  , finalizeSim
  , initSimEnv
  ) where

import           Control.Exception                 (throw)
import           Control.Monad                     (foldM, forM, forM_, mapM_,
                                                    replicateM, replicateM_,
                                                    unless, when, zipWithM)
import           Control.Monad.Reader              (MonadReader, ReaderT, asks,
                                                    local, runReaderT)
import           Data.Bits                         (complement, shiftL, shiftR,
                                                    xor, (.&.), (.|.))
import           Data.IORef                        (IORef, modifyIORef',
                                                    newIORef, readIORef,
                                                    writeIORef)
import           Data.List                         (intercalate, nub, sort,
                                                    sortBy, sortOn)
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as N
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    mapMaybe)

import           Control.Monad.Except              (MonadError)
import           Control.Monad.Extra               (concatForM, concatMapM,
                                                    mapMaybeM, unlessM, whenM)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.State               (MonadState, get, gets,
                                                    modify)
import           Data.Graph.Inductive.Graph        (LNode, lab, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.DFS    (scc, topsort)
import qualified Data.HashMap.Strict               as M
import           Data.Loc                          (noLoc)
import qualified Data.Map.Strict                   as MM
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Vector                       (fromList, (!), (//))

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.API.Internal
import           SME.CsvWriter
import           SME.Error
import           SME.Representation
import           SME.Util

--import           Debug.Trace
--import           Text.Show.Pretty                  (ppShow)
trace :: String -> a -> a
trace _ = id

traceM :: (Applicative f) => String -> f ()
traceM _ = pure ()


-- import SME.APITypes

type Env = BaseEnv Void
type DefType = BaseDefType SimExt
type TopDef = BaseTopDef SimExt
type SimEnv = BaseEnv SimExt

newtype SimM a = SimM
  { unSimM :: ReaderT Context (ReprM IO SimExt) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Context
             , MonadState SimEnv
             , MonadError TypeCheckErrors
             , MonadIO
             )

instance (MonadRepr SimExt) SimM where
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
              inst@InstDef {} -> pure (is, inst)
              ParamDef {paramType = BusPar {..}}
                --trace "GOt back ParamDef" $
               ->
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
            --trace "lookup recursing2" $
            withScope i (go (N.fromList is))


data Context = Context
  { parent     :: Maybe Ident
  , breakGuard :: Maybe Ident
  }

mkContext :: Context
mkContext = Context {parent = Nothing, breakGuard = Nothing}

withParent :: Ident -> SimM a -> SimM a
withParent x = local (\e -> e {parent = Just x})

withBreakGuard :: SimM a -> SimM a
withBreakGuard act = do
  brkLab <- getFreshLabel
  let bName = Ident (T.pack $ "__break" ++ show brkLab) noLoc
  withLocalVar bName (toValue False) $
    local (\e -> e {breakGuard = Just bName}) act

getBreakGuard :: SimM (Maybe Ident)
getBreakGuard = asks breakGuard

getParent :: SimM (Maybe Ident)
getParent = asks parent

newtype InstGraph = InstGraph
  { unInstGraph :: Gr Ident Ident
  }

data SimExt
  = EnvExt { labelSource :: !Int
           , curVtable   :: VTable
           , links       :: [ProcLink]
           , puppetMode  :: Maybe SmeCtxPtr
           , simProcs    :: [IORef ProcInst]
           , simBuses    :: [BusInst]
           , csvWriter   :: Maybe CsvChan
           -- ^ Set to true if we are run as a library
           }
           --, netGraph    ::  NetGraph}
  | ProcTabExt { nodeId             :: Int
               , uniqueBusInstances :: Int -- TODO: add unique buses here
                }
  | NetTabExt --valueTab :: M.HashMap String Value
     { --instTab :: M.HashMap Ident ProcInst
      nodeId  :: Int }
  | InstExt { nodeId :: Int }
  | EmptyExt

instance Extension SimExt where
  emptyExt = EmptyExt
  topExt = ext :: TopDef -> SimExt
  defExt = ext :: DefType -> SimExt
  envExt = ext :: SimEnv -> SimExt

data BusChan
  = LocalChan { name       :: Ident
              , maxBusVal  :: IORef Value
              , localRead  :: IORef Value
              , localWrite :: IORef Value }
  | ExternalChan { name      :: Ident
                 , maxBusVal :: IORef Value
                 , extWrite  :: ValuePtr
                 , extRead   :: ValuePtr }
  deriving (Eq)

data BusInst  = BusInst
  { chans       :: MM.Map Ident BusChan
  , busInstName :: Ref
  , ref         :: Ref -- ^Reference to the bus that this was instantiated from
  , readers     :: [Int] -- ^ Processes connected to the read end of the bus
  , writers     :: [Int] -- ^ Processes connected to the write end of the bus
  -- TODO: Implement the readers/writers thing and us it to build a graph
  } deriving (Eq)

instance Show BusInst where
  show BusInst {ref = ref} = "BusInst: " ++ show ref

-- data ParamVal

-- data ParamVal = ParamVal
--                 { IORef
--  }

data ProcInst = ProcInst
  {
  --, params    :: [(Ref, ParamVal)]
   valueTab     :: VTable
  , stmts       :: [Statement]
  , instNodeId  :: Int
  , synchronous :: Bool
  , fromEnt     :: Ident
  }
  deriving (Show, Eq)

-- TODO: Using this type for the simulation processes actually isn't such a bad
--idea.
-- type RunM = StateT ProcInst IO

type VTable =  M.HashMap Ident SimRef

data SimRef
  = MutVal { _cur   :: Value
           , maxVal :: Value }
  | ConstVal Value
  | InstVal ProcInst
  | BusVal BusInst
  deriving (Eq)

instance Show SimRef where
  show (BusVal _)   = "BusVal"
  show (ConstVal v) = show v
  show (MutVal v _) = show v
  show (InstVal v)  = show v

-- | Runs a ProcInst and saves its modified vtable
runProcess :: ProcInst -> SimM ProcInst
runProcess p@ProcInst {..} = do
  (vtab, _) <- withScope fromEnt $ withVtable valueTab $ mapM_ evalStm stmts
  return $ p {valueTab = vtab}

-- | Evaluates a statement
evalStm :: Statement -> SimM ()
evalStm Assign {..} = do
  r <- evalExpr val
  ty <- lookupTy dest
  setValueVtab dest (truncateAsType r ty)
evalStm If {..} = do
  c <- evalCondPair [(cond, body)]
  unless c $ do
    c' <- evalCondPair elif
    case els of
      Nothing -> return ()
      Just ss -> unless c' (mapM_ evalStm ss)
  where
    evalCondPair ((e, ss):conds) = do
      c <-
        evalExpr e >>= \case
          (BoolVal v) -> pure v
          _ -> throw $ InternalCompilerError "Type error in if"
      if c
        then do
          mapM_ evalStm ss
          return c
        else evalCondPair conds
    evalCondPair [] = return False

evalStm Switch {..} = do
  val <- evalExpr value
  res <- evalCase val cases
  unless res $ fromMaybe (return ()) (mapM_ evalStm <$> defaultCase)
  where
    evalCase _ [] = return False
    evalCase sVal ((expr, stms):ss) = do
      e <- evalExpr expr
      if e == sVal
        then do
          mapM_ evalStm stms
          return True
        else evalCase sVal ss

evalStm Trace {..} =
  case str of
    LitString {stringVal = stringVal} ->
      unlessM (getConfig quiet) $
      if null subs
        then liftIO $ T.putStrLn stringVal
        else do
          let s = T.splitOn "{}" stringVal
          vals <- map pprr <$> mapM evalExpr subs
          let res = mconcat $ zipWith (<>) s vals
          liftIO $ T.putStrLn res
    _ -> throw $ InternalCompilerError "Not a string lit"

evalStm Assert {..} =
  unlessM (getConfig noAsserts) $ do
    str <-
      case descr of
        Nothing -> return Nothing
        Just LitString {stringVal = stringVal} -> return (Just stringVal)
        _ -> throw $ InternalCompilerError "Not a string lit"
    evalExpr cond >>= \case
      (BoolVal False) -> throw $ AssertionError cond (T.unpack <$> str)
      (BoolVal True) -> return ()
      _ -> throw $ InternalCompilerError "Assertion not a boolean"

evalStm For {..} = do
  start <- evalExpr from
  end <- evalExpr to
  let ops =
        if start <= end
          then (PlusOp noLoc, GeqOp noLoc)
          else (MinusOp noLoc, LeqOp noLoc)
  end' <- evalBinOp (fst ops) end (toValue (1 :: Integer))
  -- We abuse the vtable a bit here and use it to hold a "breakguard" variable
  -- which is set to true by the break statement.
  -- FIXME: Handle this in a nicer way
  withBreakGuard $
    withLocalVar var start $ loop ops start end' (identToName var)
  where
    checkBreakGuard =
      getBreakGuard >>= \case
        Nothing -> return False
        Just g -> do
          val <- getValueVtab $ identToName g
          valueToBoolE =<< evalBinOp (EqOp noLoc) val (toValue True)
    evalStmsCheckGuard [] = return True
    evalStmsCheckGuard (s:stms) = do
      brk <- checkBreakGuard
      if brk
        then return False
        else do
          evalStm s
          evalStmsCheckGuard stms
    loop ops@(incrOp, cmpOp) cur end counter =
      whenM (evalStmsCheckGuard body) $ do
        cur' <- evalBinOp incrOp cur (toValue (1 :: Integer))
        setValueVtab counter cur'
        unlessM
          (valueToBoolE =<< evalBinOp cmpOp cur' end)
          (loop ops cur' end counter)

evalStm Break {} =
  getBreakGuard >>= \case
    Nothing -> throw $ InternalCompilerError "Break called outside loop"
    Just b -> setValueVtab (identToName b) (toValue True)

evalStm _ = error "Simulation of all statements not implemented yet"

valueToBoolE :: Value -> SimM Bool
valueToBoolE (BoolVal v) = pure v
valueToBoolE _           = throw $ InternalCompilerError "Expected bool"

-- | Evaluates an expression
evalExpr :: Expr -> SimM Value
evalExpr Binary {..} = do
  l <- evalExpr left
  r <- evalExpr right
  evalBinOp binOp l r
evalExpr Unary {..} = do
  r <- evalExpr right
  evalUnOp unOp r
evalExpr PrimName {..} =
  getValueVtab name
evalExpr PrimLit {..} =
  pure $ toValue lit
evalExpr Parens {..} = evalExpr innerExpr
evalExpr _ = error "Function calls not implemented yet"

-- TODO: Find a better way of doing this
evalBinOp :: BinOp -> Value -> Value -> SimM Value
evalBinOp PlusOp  {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i + j
evalBinOp PlusOp  {} (DoubleVal i) (DoubleVal j)   = pure $ DoubleVal $ i + j
evalBinOp PlusOp  {} (SingleVal i) (SingleVal j)   = pure $ SingleVal $ i + j
evalBinOp MinusOp {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i - j
evalBinOp MinusOp {} (DoubleVal i) (DoubleVal j)   = pure $ DoubleVal $ i - j
evalBinOp MinusOp {} (SingleVal i) (SingleVal j)   = pure $ SingleVal $ i - j
evalBinOp ModOp   {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i `mod` j
evalBinOp MulOp   {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i * j
evalBinOp MulOp   {} (DoubleVal i) (DoubleVal j)   = pure $ DoubleVal $ i * j
evalBinOp MulOp   {} (SingleVal i) (SingleVal j)   = pure $ SingleVal $ i * j
evalBinOp DivOp   {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i `div` j
evalBinOp DivOp   {} (DoubleVal i) (DoubleVal j)   = pure $ DoubleVal $ i / j
evalBinOp DivOp   {} (SingleVal i) (SingleVal j)   = pure $ SingleVal $ i / j
evalBinOp AndOp   {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i .&. j
evalBinOp OrOp    {} (IntVal i)    (IntVal j)      = pure $ IntVal $ i .|. j
evalBinOp SllOp   {} (IntVal i)    (IntVal j)      =
  pure $ IntVal $  shiftL i (fromIntegral j)
evalBinOp SrlOp   {} (IntVal i) (IntVal j)         =
  pure $ IntVal $  shiftR i (fromIntegral j)
evalBinOp XorOp   {} (IntVal i) (IntVal j)         = pure $ IntVal $ i `xor` j
evalBinOp ConOp   {} (BoolVal i)    (BoolVal j)    = pure $ BoolVal $ i && j
evalBinOp EqOp    {} (IntVal i)    (IntVal j)      = pure $ BoolVal $ i == j
evalBinOp EqOp    {} (DoubleVal i) (DoubleVal j)   = pure $ BoolVal $ i == j
evalBinOp EqOp    {} (SingleVal i) (SingleVal j)   = pure $ BoolVal $ i == j
evalBinOp EqOp    {} (BoolVal i)   (BoolVal j)     = pure $ BoolVal $ i == j
evalBinOp DisOp   {} (BoolVal i)   (BoolVal j)     = pure $ BoolVal $ i || j
evalBinOp GeqOp   {} (IntVal i)    (IntVal j)      = pure $ BoolVal $ i >= j
evalBinOp GeqOp   {} (DoubleVal i) (DoubleVal j)   = pure $ BoolVal $ i >= j
evalBinOp GeqOp   {} (SingleVal i) (SingleVal j)   = pure $ BoolVal $ i >= j
evalBinOp GeqOp   {} (BoolVal i) (BoolVal j)       = pure $ BoolVal $ i >= j
evalBinOp GtOp    {} (IntVal i) (IntVal j)          = pure $ BoolVal $ i > j
evalBinOp GtOp {} (DoubleVal i) (DoubleVal j)    = pure $ BoolVal $ i > j
evalBinOp GtOp {} (SingleVal i) (SingleVal j)    = pure $ BoolVal $ i > j
evalBinOp GtOp {} (BoolVal i) (BoolVal j)        = pure $ BoolVal $ i > j
evalBinOp LeqOp {} (IntVal i) (IntVal j)         = pure $ BoolVal $ i <= j
evalBinOp LeqOp {} (DoubleVal i) (DoubleVal j)   = pure $ BoolVal $ i <= j
evalBinOp LeqOp {} (SingleVal i) (SingleVal j)   = pure $ BoolVal $ i <= j
evalBinOp LeqOp {} (BoolVal i) (BoolVal j)       = pure $ BoolVal $ i <= j
evalBinOp LtOp {} (IntVal i) (IntVal j)          = pure $ BoolVal $ i < j
evalBinOp LtOp {} (DoubleVal i) (DoubleVal j)    = pure $ BoolVal $ i < j
evalBinOp LtOp {} (SingleVal i) (SingleVal j)    = pure $ BoolVal $ i < j
evalBinOp LtOp {} (BoolVal i) (BoolVal j)        = pure $ BoolVal $ i < j
evalBinOp NeqOp {} (IntVal i) (IntVal j)         = pure $ BoolVal $ i /= j
evalBinOp NeqOp {} (DoubleVal i) (DoubleVal j)   = pure $ BoolVal $ i /= j
evalBinOp NeqOp {} (SingleVal i) (SingleVal j)   = pure $ BoolVal $ i /= j
evalBinOp NeqOp {} (BoolVal i) (BoolVal j)       = pure $ BoolVal $ i /= j
evalBinOp o v1 v2 =
  throw $
  InternalCompilerError
    ("Unsupported types for binary operator " ++
     show o ++ " " ++ show v1 ++ " " ++ show v2)

evalUnOp :: UnOp -> Value -> SimM Value
evalUnOp UnPlus {} (IntVal i)     = pure $ IntVal i
evalUnOp UnPlus {} (SingleVal i)  = pure $ SingleVal i
evalUnOp UnPlus {} (DoubleVal i)  = pure $ DoubleVal i
evalUnOp UnMinus {} (IntVal i)    = pure $ IntVal $ negate i
evalUnOp UnMinus {} (SingleVal i) = pure $ SingleVal $ negate i
evalUnOp UnMinus {} (DoubleVal i) = pure $ DoubleVal $ negate i
evalUnOp NotOp {} (BoolVal i)     = pure $ BoolVal $ not i
evalUnOp NegOp {} (IntVal i)      = pure $ IntVal $ complement i
evalUnOp _ _                      =
  throw $ InternalCompilerError "Unsupported types for unary operator"


propagateBus :: BusInst -> SimM  [Value]
propagateBus BusInst {..}
 = do
  --liftIO $ putStrLn ("Propagating bus " ++ show ref)
  let vs = MM.elems chans
  forM
    vs
    (\case
       LocalChan {localRead = readRef, localWrite = writeRef} ->
         liftIO $
            --traceM ("Bus looks like " ++ show name)
          do
           rw <- readIORef writeRef
           --rv <- readIORef readRef
           writeIORef readRef rw
           --putStrLn ("Bus read value was " ++ show rv)
           return rw
       ExternalChan {extRead = readEnd}
          -- External channels are propagated by the c-wrapper
        -> liftIO $ peek readEnd)


-- | Returns a new and globally unique integer every time its called.
getFreshLabel :: SimM Int
getFreshLabel = do
  curEnv <- get
  let curExt = ext (curEnv :: SimEnv)
      nextId = labelSource curExt
  modify (\x -> x {ext = curExt {labelSource = nextId + 1}} :: SimEnv)
  return nextId

-- | Assign a unique label for every instance. Used for graph construction.
labelInstances :: SimM ()
labelInstances = mapUsedTopDefsM_ go
  where
    go ProcessTable {..} = do
      symTab' <- mapM go' symTable
      newLab <- getFreshLabel
      updateTopDef
        procName
        (\x ->
           x
           { symTable = symTab'
           , ext = ProcTabExt {uniqueBusInstances = 0, nodeId = newLab}
           })
    go NetworkTable {..} = do
      symTab' <- mapM go' symTable
      newLab <- getFreshLabel
      updateTopDef
        netName
        (\x ->
           x
           { symTable = symTab'
           , ext = NetTabExt {nodeId = newLab} ---, valueTab = M.empty}
           })
    go' i@InstDef {} = do
      newLab <- getFreshLabel
      return ((i {ext = InstExt {nodeId = newLab}}) :: DefType)
    go' i = return i

class ToValue a where
  toValue :: a -> Value

instance ToValue Literal where
  toValue LitInt {..} = IntVal intVal
  -- FIXME: This calls for changing the representation of floating point values in
  -- the AST to something completely accurate.
  toValue LitFloat {} = undefined
  toValue LitArray {..} =
    ArrayVal (length arrayVal) $ fromList (map toValue arrayVal)
  toValue LitString {} = undefined
  toValue LitTrue {} = BoolVal True
  toValue LitFalse {} = BoolVal False

instance ToValue Int where
  toValue v = IntVal $ fromIntegral v

instance ToValue Integer where
  toValue v = IntVal $ fromIntegral v

instance ToValue Double where
  toValue = DoubleVal

instance ToValue Float where
  toValue = SingleVal

instance ToValue Bool where
  toValue = BoolVal

instance (ToValue a) => ToValue [a] where
  toValue v = let list = fromList (map toValue v)
              in ArrayVal (length v) list

instance ToValue Value where
  toValue = id


-- | Get the Type of a Value. Will try to "adapt" the type to the given
-- Typeness parameter such that it, e.g., returns signed types if existing type
-- is signed
valueToType :: Value -> Typeness -> Typeness
valueToType (IntVal v) (Typed Signed {..})   =
  Typed $ Signed (Just (bitSize v + 1)) loc
valueToType (IntVal v) (Typed Unsigned {..}) =
  Typed $ Unsigned (Just (bitSize v)) loc
valueToType v (Typed Array {arrLength = al, innerTy = iTy}) =
  let arrTy =
        case valueToType v (Typed iTy) of
          Typed t' -> t'
          Untyped  -> error "Untyped" -- This should never happen
  in Typed $ Array al arrTy noLoc
valueToType (IntVal v) _
  -- FIXME: What to do here? Either we fail since an internal invariant has been
  -- violated or we stick with the current implementation
 = typeOf v
valueToType (BoolVal _) t                      = t
valueToType (SingleVal _) t                    = t
valueToType (DoubleVal _) t                    = t
valueToType (ArrayVal _ v) t = valueToType (maximum v) t

-- | Creates a bus instance either locally or in the C-interface depending on if
-- the bus is stored locally or not.
mkBusInst :: Bool -> Ref -> BusShape -> Ref -> SimM BusInst
mkBusInst exposed n bs busRef = do
  chans <-
    puppetMode <$> gets (ext :: SimEnv -> SimExt) >>= \case
      Just ptr -> do
        busPtr <- liftIO $ mkExtBus ptr (pprrString n)
        toExtChans exposed busPtr bs
      Nothing -> toBusChans bs
  let res = BusInst (MM.fromList (sortOn fst chans)) n busRef [] []
  addBusInst res
  return res
  where
    toExtChans :: Bool -> BusPtr -> BusShape -> SimM [(Ident, BusChan)]
    toExtChans isPuppet bptr bs' =
      forM
        (unBusShape bs')
        (\case
           (i, (oTy@(Typed ty), lit, _)) -> do
             defVal <- genDefaultValue lit oTy
             (i, ) <$>
               if isPuppet
                 then liftIO $ do
                        chan <- mkExtChan bptr (toString i) ty
                        ExternalChan i <$> newIORef defVal <*>
                          pure (writePtr chan) <*>
                          pure (readPtr chan)
                 else liftIO $
                      LocalChan i <$> newIORef defVal <*> newIORef defVal <*>
                      newIORef defVal
           _ -> throw $ InternalCompilerError "Illegal bus chan")
    toBusChans :: BusShape -> SimM [(Ident, BusChan)]
    toBusChans bs' =
      mapM
        (\(i, (ty, lit, _)) -> do
           defVal <- genDefaultValue lit ty
           (i, ) <$>
             liftIO
               (LocalChan i <$> newIORef defVal <*> newIORef defVal <*>
                newIORef defVal))
        (unBusShape bs')


genDefaultValue :: Maybe Literal -> Typeness -> SimM Value
genDefaultValue Nothing ty = mkInitialValue ty
genDefaultValue (Just l) _ = return $ toValue l

mkInitialValue :: Typeness -> SimM Value
mkInitialValue Untyped = throw $ InternalCompilerError "Untyped value in sim"
mkInitialValue (Typed t) = go t
  where
    go Signed {} = return $ toValue (0 :: Int)
    go Unsigned {} = return $ toValue (0 :: Int)
    go Single {} = return $ toValue (0 :: Float)
    go Double {} = return $ toValue (0 :: Double)
    go Bool {} = return $ toValue False
    go String {} = undefined
    go Array {..} = do
      len <-
        case arrLength of
          Just e ->
            evalConstExpr e >>= \case
              IntVal v -> pure (fromIntegral v)
              _ -> throw $ InternalCompilerError "Non-integer array length"
          Nothing -> throw $ InternalCompilerError "No array len" -- FIXME
      toValue <$>
        replicateM len (mkInitialValue (typeOf innerTy))


-- | Creates a new vtable @ds@ from a list of definitions, adding to table
-- passed as 'vtab'
mkVtable :: [DefType] -> VTable -> SimM VTable
mkVtable ds vtab = foldM go vtab ds
  where
    go m VarDef {..} = do
      (defaultValue, maxValue) <-
        case varVal of
          Just v ->
            case toValue v of
              a@(ArrayVal _ av) -> pure (a, maximum av)
              v'                -> pure (v', v')
          Nothing -> do
            v <- mkInitialValue (typeOf varDef)
            pure (v, v)
      return $ M.insert varName (MutVal defaultValue (absValue maxValue)) m
    go m ConstDef {..} =
      return $ M.insert constName (ConstVal $ toValue constVal) m
    go m EnumFieldDef {..} =
      return $ M.insert fieldName (ConstVal $ toValue fieldValue) m
    go m BusDef {..} = do
      bn <-
        getParent >>= \case
          Just p -> pure $ refOf p <> refOf busName
          Nothing -> pure $ refOf busName
      bus <- mkBusInst isExposed bn busShape busRef
      return $ M.insert busName (BusVal bus) m
    go m _ = return m


-- | Checks for cycles in the instantiation graph by calculating the Strongly
-- Connected Components (SCC) of the graph. Cycles are indicated by the presence
-- of SCCs consisting of more than one node.
ensureAcyclic :: InstGraph -> SimM ()
ensureAcyclic g =
  case filter ((> 1) . length) (scc (unInstGraph g)) of
    [] -> return ()
  -- TODO: Better error message with line numbers
    a ->
      let labs = (map . map) (lab (unInstGraph g)) a
          labs' = (map . map) (fromMaybe (Ident "unknown" noLoc)) labs
      in throw $
         -- TODO: This should have its own error type
         InternalCompilerError
           ("Instantiation cycles formed by entities " ++
            unwords (map show labs'))


-- | Returns a list of processes that is instantiated from a process.
instantiates :: Ident -> [DefType] -> SimM [LNode Ident]
instantiates instantiator = mapMaybeM go
  where
    go InstDef {..} = do
      td <- lookupTopDef instantiated
      when (instantiator == instantiated) $
        -- TODO: We should make sure this is not the case in the typechecker
        throw $ InternalCompilerError "Entity cannot instantiate itself"
        -- TODO: Better error
      return $ Just (nodeId (topExt td), instantiated)
    go _ = return Nothing


-- | Finds the entity that instantiation should start from. Generates the
-- instantiation graph and ensures that it is acyclic. The returned element is
-- the first element obtained by topologically sorting the nodes of the graph.
getNetworkEntry :: SimM Ident
getNetworkEntry = do
  edges' <- concat . nub <$> mapUsedTopDefsM go
  let nodes' = concatMap (\(e1, e2) -> [e1, e2]) edges'
      edges'' =
        map
          (\((n1, s1), (n2, s2)) -> (n1, n2, s1 <> Ident "_" noLoc <> s2))
          edges'
      graph = InstGraph $ mkGraph nodes' edges''
      instOrder = topsort (unInstGraph graph)
  entryProc <-
    case instOrder of
      (x:_) -> pure x
      []    -> throw $ InternalCompilerError "Network contains no processes"
  -- liftIO $ prettyPrint (unInstGraph graph)
  -- liftIO $ print $ isConnected (unInstGraph graph)
  -- liftIO $ print $ topsort (unInstGraph graph)
  -- liftIO $ print $ scc (unInstGraph graph)
  ensureAcyclic graph
  return $
    fromMaybe (Ident "__unknown" noLoc) $ lab (unInstGraph graph) entryProc
  where
    go a = do
      let symtab = symTable a
      insts <- instantiates (nameOf a) (M.elems symtab)
      return $ map ((nodeId (topExt a), nameOf a), ) insts

-- | Run the action @act@ setting the current vTable to @vtab@. Returns the
-- modified vTable and the result of the action
withVtable :: VTable -> SimM a -> SimM (VTable, a)
withVtable vtab act = do
  e <- gets (ext :: SimEnv -> SimExt)
  let prev = curVtable e
  modify (\x -> x {ext = e {curVtable = vtab}} :: SimEnv)
  res <- act
  e' <- gets (ext :: SimEnv -> SimExt)
  modify (\x -> x {ext = e' {curVtable = prev}} :: SimEnv)
  return (curVtable e', res)

-- | Same as withVTable, but doesn't return the updated vTable.
withVtable_ :: VTable -> SimM a -> SimM a
withVtable_ vtab act = snd <$> withVtable vtab act

-- | Return the current vTable
getCurVtable :: SimM VTable
getCurVtable = curVtable <$> gets (ext :: SimEnv -> SimExt)

addCurVtable :: Ident -> SimRef -> SimM ()
addCurVtable i r = do
  vtab <- getCurVtable
  putCurVtable $ M.insert i r vtab

-- | Performs the computation 'act' with the variable 'i' present in the vtable
-- initially assigned to value 'r'
withLocalVar :: Ident -> Value -> SimM a -> SimM a
withLocalVar i r act = do
  addCurVtable i (MutVal r r)
  res <- act
  putCurVtable . M.delete i =<< getCurVtable
  return res

putCurVtable :: VTable -> SimM ()
putCurVtable vtab = do
  e <- gets (ext :: SimEnv -> SimExt)
  modify (\x -> x {ext = e {curVtable = vtab}} :: SimEnv)

addLink :: ProcLink -> SimM ()
addLink l = do
  e <- gets (ext :: SimEnv -> SimExt)
  let ls = links e
  modify (\x -> x {ext = e {links = l:ls}} :: SimEnv)

-- We may use this later
-- getLinks :: SimM [ProcLink]
-- getLinks = links <$> gets (ext :: SimEnv -> SimExt)

addBusInst :: BusInst -> SimM ()
addBusInst bi = do
  e <- gets (ext :: SimEnv -> SimExt)
  let insts = simBuses e
  -- FIXME: This is a bit inefficient
  modify (\x -> x {ext = e {simBuses = sortOn busInstName (bi : insts)}} :: SimEnv)

lookupCurVtable :: Ident -> SimM (Maybe SimRef)
lookupCurVtable i = do
  e <- gets (ext :: SimEnv -> SimExt)
  --traceM $ "Cur Vtable is " ++ show (curVtable e)
  return $ M.lookup i (curVtable e)
    -- Nothing -> return Nothing
    -- v       -> return v

lookupCurVtableE :: Ident -> SimM SimRef
lookupCurVtableE i =
  --traceM ("LoockupCurVtaleE called with " ++ show i)
  lookupCurVtable i >>= \case
    Just v -> return v
    Nothing -> throw $ InternalCompilerError "Undefined name during simulation"

setInVtab :: Ident -> SimRef -> SimM ()
setInVtab i v = getCurVtable >>= pure . M.insert i v >>= putCurVtable

getInVtab ::  Ident -> SimM SimRef
getInVtab i =
  (M.lookup i <$> getCurVtable) >>= \case
    Just v -> pure v
    Nothing -> throw $ InternalCompilerError ("Value not found " ++ show i)

-- | Sets a value in current VTab
setValueVtab :: Name -> Value -> SimM ()
setValueVtab Name {parts = parts} v' = go parts
  where
    go (p :| []) =
      case p of
        IdentName {ident = i} ->
          getInVtab i >>= \case
            MutVal _ maxV -> setInVtab i $ MutVal v' (max maxV (absValue v'))
            BusVal _ -> throw $ InternalCompilerError "bus as value"
            _ -> throw $ InternalCompilerError "immutable value"
        ArrayAccess {..} -> do
          i <- ensureNamePartIdent namePart
          idx <- ensureIndex index
          getInVtab i >>= \case
            MutVal (ArrayVal l v) m ->
              if idx > (fromIntegral l - 1)
                -- TODO: Dedicated error type
                then throw $ InternalCompilerError "Index out of bounds"
                else setInVtab i $
                     MutVal
                       (ArrayVal l (v // [(idx, v')]))
                       (max v' (absValue m))
            ConstVal ArrayVal {} ->
              throw $ InternalCompilerError "immutable array"
            _ -> throw $ InternalCompilerError "Non-array value"
    go (i :| [i2]) = do
      i' <- ensureNamePartIdent i
      i2' <- ensureNamePartIdent i2
      getInVtab i' >>= \case
        BusVal v -> setBusVal i2' v v'
        _ -> throw $ InternalCompilerError "compund name not a bus"
    go _ = throw $ InternalCompilerError "Compound names not supported"

-- FIXME: We should probably introduce an intermediate language to avoid
-- handling this here
ensureIndex :: ArrayIndex -> SimM Int
ensureIndex (Index e) = evalExpr e >>= \case
  IntVal v -> return (fromIntegral v)
  _ -> -- TODO: Check this in the typechecker
    throw $ InternalCompilerError "Non-integer array index"
ensureIndex Wildcard = throw $ InternalCompilerError "Wildcard index"

ensureNamePartIdent :: NamePart -> SimM Ident
ensureNamePartIdent IdentName {..} = return ident
ensureNamePartIdent ArrayAccess {} =
  throw $ InternalCompilerError "ArrayAccess array NamePart"

getValueVtab :: Name -> SimM Value
getValueVtab Name {parts = parts} = go parts
  where
    go (p :| []) =
      case p of
        IdentName {ident = i} ->
          getInVtab i >>= \case
            MutVal v _ -> pure v
            ConstVal v -> pure v
            _ -> throw $ InternalCompilerError "not readable"
        ArrayAccess {..} -> do
          i <- ensureNamePartIdent namePart
          idx <- ensureIndex index
          getInVtab i >>= \case
            MutVal (ArrayVal l v) _ -> readArray idx l v
            ConstVal (ArrayVal l v) -> readArray idx l v
            _ -> throw $ InternalCompilerError "Non-array value"
      where
        readArray idx l v =
          if idx > (fromIntegral l - 1)
                 -- TODO: Dedicated error type
            then throw $ InternalCompilerError "Index out of bounds"
            else return $ v ! fromIntegral idx
    go (p :| [p2])
      -- FIXME: This is a hack
     = do
      i <- ensureNamePartIdent p
      i2 <- ensureNamePartIdent p2
      getInVtab i >>= \case
        BusVal v -> getBusVal i2 v
        _ ->
          throw $
          InternalCompilerError
            "Only buses are accessible through compound names"
    go _ = throw $ InternalCompilerError "Compound names not supported"

-- valAToSimRef :: Value -> SimM SimRef
-- valToSimRef = undefined

-- TODO: Maybe replace these with Storable instances for BusChan

setBusVal :: Ident -> BusInst -> Value -> SimM ()
setBusVal i BusInst {..} v =
  case MM.lookup i chans of
    Just LocalChan {localWrite = write, maxBusVal = maxBusVal} ->
      liftIO $ do
        modifyIORef' maxBusVal (`max` absValue v)
        writeIORef write v
    Just ExternalChan {extWrite = write, maxBusVal = maxBusVal} -> do
      traceM ("Writing external channel " ++ show v)
      liftIO $ do
        modifyIORef' maxBusVal (`max` absValue v)
        poke write v
    Nothing -> throw $ InternalCompilerError "undefined bus channel"

getBusVal :: Ident -> BusInst -> SimM Value
getBusVal i BusInst {..} = do
  traceM $ "Reading bus val " ++ show i
  case MM.lookup i chans of
    Just LocalChan {localRead = readEnd} -> liftIO $ readIORef readEnd
    Just ExternalChan {extRead = readEnd, maxBusVal = maxBV} -> do
      res <- liftIO $ peek readEnd
      -- TODO: Handle this on propagation rather than doing it on every read
      liftIO $ modifyIORef' maxBV (`max` res)
      trace ("Reading external channel " ++ show res ++ " " ++ show i) $
        return res
    Nothing -> throw $ InternalCompilerError "undefined bus channel"

-- | Converts a simulator value reference to a value.
getValue :: SimRef -> SimM Value
getValue (MutVal v _ ) = return v
getValue (ConstVal v)  = return v
getValue _ = throw $ InternalCompilerError "SimRef does not have a simple value"

evalConstExpr :: Expr -> SimM Value
evalConstExpr PrimLit {..} = return $ toValue lit
evalConstExpr PrimName {name = n@Name {..}} =
  case parts of
    (IdentName {ident = ident} :| _) -> getValue =<< lookupCurVtableE ident
    (ArrayAccess {} :| _)            -> getValueVtab n
evalConstExpr _ = error "TODO: Better constexpr evaluation"
    -- do
    -- lookupCurVtable ident >>= \case
    --   ArrayVal {} -> error "Arrays not implemented"

-- mkInitialVtable :: [(Ident, SimRef)] -> VTable
-- mkInitialVtable = M.fromList

-- getBusInstRef :: Ref -> SimM (IORef BusInst)
-- getBusInstRef (b :| _) = do
--   e <- gets (ext :: SimEnv -> SimExt)
--   case M.lookup b (curVtable e) of
--     Just (BusVal r) -> return r
--     Nothing -> throw $ InternalCompilerError "Undefined bus during simulation"


--getBusReference

-- TODO: Normalize the SMEIL code by rewriting it such that direct references to
-- top-level entity definitions are transformed such that the anonymous
-- instances are passed either as input or output parameters to the
-- processes. As a first attempt. Focus only on implementing code generation for
-- programs like in the final form of addone.sme, since that is similar to the
-- normalized form that we want to end up with.

-- *** Network instantiation and initialization

newtype ProcLink = ProcLink (Int, Int, String, BusInst)

data InstTree = InstTree
  { node  :: [ProcInst]
  -- ^ Contains a bus connection
  , leafs :: [InstTree]
  }
  deriving (Eq, Show)

instance Semigroup InstTree where
  (InstTree na ca) <> (InstTree nb cb) = InstTree (na <> nb) (ca <> cb)

instance Monoid InstTree where
  mempty = InstTree [] []

flattenInstTree :: InstTree -> [ProcInst]
flattenInstTree (InstTree n []) = n
flattenInstTree (InstTree n c)  = n ++ concatMap flattenInstTree c

-- | Looks up the bus value corresponding to a bus reference and also returns
-- the instance id for the bus that the process in declared in.
resolveBusParam :: Ref -> SimM (Int, BusInst)
resolveBusParam = go 0
  -- FIXME: In some cases we will actually get a bus back from a single name. In
  -- this case, the node reference returned will be invalid
  where
    go n (r :| []) =
      lookupCurVtableE r >>= \case
        BusVal res -> return (n, res)
        _ -> throw $ InternalCompilerError "Expected bus"
    go _ (r :| rs) =
      lookupCurVtableE r >>= \case
        InstVal instv
        --inst <- liftIO $ readIORef r
         -> withVtable_ (valueTab instv) $ go (instNodeId instv) (N.fromList rs)
        _ -> throw $ InternalCompilerError "Expected instance"

-- | Entity instantiation function. Recursively walks through the instantiation
-- hierachy. Takes a pre-populated symbol table and an entity as argument
instEntity :: VTable -> TopDef -> SimM InstTree
instEntity st NetworkTable {netName = name, symTable = symTable} = do
  let symtab = M.elems symTable
  vtab <- mkVtable symtab st
  --traceM $ "Made symtab: " ++ show vtab
  withVtable_ vtab $ withScope name $ processInstDefs symtab
instEntity st ProcessTable {stms = stms, procName = name, symTable = symTable} = do
  let symtab = M.elems symTable
  vtab <- mkVtable symtab st
  instTree <- withVtable_ vtab $ withScope name $ processInstDefs symtab
  --vtab <- mkVtable (M.elems symTable) (mkInitialVtable paramVals)
  newLab <- getFreshLabel
  let inst =
        ProcInst
        { valueTab = vtab
        , stmts = stms
        , instNodeId = newLab
        , fromEnt = name
        , synchronous = False -- TODO
        }
  return $ instTree {node = [inst]}

processInstDefs :: [DefType] -> SimM InstTree
processInstDefs dt = do
  (instDefs, insts) <- unzip . catMaybes <$> mapM mkInst dt
  let (InstTree myInsts leafs) = mconcat insts
  mapM_ (uncurry addCurVtable) (zip instDefs (map InstVal myInsts))
  myInsts' <- mapM wireInst (zip instDefs myInsts)
  let instTree = InstTree myInsts' leafs
  return (InstTree [] [instTree])

mkInst :: DefType -> SimM (Maybe (Ident, InstTree))
mkInst InstDef { instantiated = instantiated
               , instDef = Instance {params = actual}
               , instName = instName
               , anonymous = anon
               }
  -- TODO: Give instances names of the format
  -- process_name-inst_name-bus_name-chan_name (This will not be unique in
  -- the case of recursive instances). We can get the top-level-entity since
  -- we are in its scope (see withScope). We also know the name of its
  -- instances (right here) and the buses since we instantiate them here
 = do
  inst <- lookupTopDef instantiated
  let parList = (params :: TopDef -> ParamList) inst
      instParent =
        if anon
          then id
          else withParent instName
  -- Evaluate the parameter values
  paramVals <-
    catMaybes <$>
    zipWithM
      (\(parName, parType) (_, parVal) ->
         case parType of
           ConstPar _ ->
             Just . (parName, ) <$> (ConstVal <$> evalConstExpr parVal)
           BusPar {} -> pure Nothing)
      parList
      actual
  instParent $ Just . (instName, ) <$> instEntity (M.fromList paramVals) inst
mkInst _ = pure Nothing

-- TODO: Handle parameters in the type check somehow
-- | Reduce an expression to a name and fail if impossible
exprReduceToName :: (MonadRepr s m) => Expr -> m Name
exprReduceToName PrimName {..} = return name
exprReduceToName e             = throw $ ExprInvalidInContext e

-- | Adds actual bus references to the vtable of a process instance
wireInst :: (Ident, ProcInst) -> SimM ProcInst
wireInst (instDefName, procInst@ProcInst {instNodeId = myNodeId})
  --traceM "Entered wireInst"
 =
  lookupDef instDefName >>= \case
    InstDef {instantiated = instantiated, instDef = Instance {params = actual}} -> do
      inst <- lookupTopDef instantiated
      let parList = (params :: TopDef -> ParamList) inst
      paramVals <-
        catMaybes <$>
        zipWithM
          (\(parName, parType) (_, parVal) ->
             case parType of
               ConstPar _ -> pure Nothing
               BusPar {..}
                 -- FIXME: This is probably completely bogus since paramRef
                 -- contains a reference used when instantiating the bus. This
                 -- has no business hanging around in the process definition.
                -> do
                 (nid, ref') <-
                   resolveBusParam =<< refOf <$> exprReduceToName parVal
                 case busState of
                   Input -> addLink (ProcLink (myNodeId, nid, "foo", ref'))
                   Output -> addLink (ProcLink (nid, myNodeId, "foo", ref'))
                   _ -> throw $ InternalCompilerError "BusState invalid here"
                 return $ Just (parName, BusVal ref'))
          parList
          actual
      let vtab = (valueTab :: ProcInst -> VTable) procInst
          vtab' = foldr (uncurry M.insert) vtab paramVals
      return $ procInst {valueTab = vtab'}
    _ -> throw $ InternalCompilerError "Expected instDef"

-- | Sets up the simulation sets up the simulation environment
setupSimEnv :: Maybe SmeCtxPtr -> SimM ()
setupSimEnv ptr = do
  --"trace.csv"
  --csv <- Nothing
  modify
    (\x ->
       x
       { ext =
           EnvExt
           { labelSource = 0
           , curVtable = M.empty
           , links = []
           , puppetMode = ptr
           , simProcs = []
           , simBuses = []
           , csvWriter = Nothing
           }
       } :: SimEnv)
  labelInstances
  --constructGraph
  entry <- getNetworkEntry
  -- Mark the entity as top-level
  -- TODO: Move the whole graph generation part out of the simulator since it is
  -- technically unrelated to the simulator
  updateTopDef entry (\x -> x {topLevel = True})
  tree <- lookupTopDef entry >>= instEntity M.empty
  let insts = flattenInstTree tree
  -- traceM $ ppShow tree
  -- FIXME: What to do here:
  -- let instMap = instsToMap insts
  -- links <- getLinks
  -- nodes' <-
  --   concat <$>
  --   mapM
  --     (\(ProcLink (n1, n2, _l, _shouldBeUsed)) -> do
  --        ref1 <- liftIO $ newIORef $ instMap M.! n1
  --        ref2 <- liftIO $ newIORef $ instMap M.! n2
  --        return [(n1, ref1), (n2, ref2)])
  --     links
  -- let edges = map (\(ProcLink (n1, n2, l, _)) -> (n1, n2, l)) links
  --     graph = ProcGraph $ mkGraph nodes' edges
  --     buses = nub $ map (\(ProcLink (_, _, _, b)) -> b) links
  procs <- liftIO $ mapM newIORef insts -- nub $ map snd nodes'
  -- Save buses in state
  modify
    (\x ->
       let ee = envExt x
       in x {ext = ee {simProcs = procs}} :: SimEnv)

  -- liftIO $ print (length buses, length procs)
  -- _ <- replicateM 10 $ runSimulation procs buses

modifyIORefM :: (MonadIO m) => (a -> m a) -> IORef a -> m ()
modifyIORefM !f r = liftIO (readIORef r) >>= f >>= (liftIO . writeIORef r)

runSimulation :: [IORef ProcInst] -> [BusInst] -> SimM ()
runSimulation procs buses = do
  mapM_ propagateBus buses
  mapM_ (modifyIORefM runProcess) procs

--setupIncrSimulation :: Env ->

initSimEnv :: Env -> SmeCtxPtr -> IO (Either TypeCheckErrors SimEnv)
initSimEnv env ptr = runStep (toSimEnv env) $  setupSimEnv (Just ptr)

newSteppingSim :: Env -> SmeCtxPtr -> IO (Either TypeCheckErrors SimEnv)
newSteppingSim env ptr =
  runStep (toSimEnv env) $ do
    setupSimEnv (Just ptr)
    csv <-
      getConfig traceFile >>= \case
        Just a -> liftIO (Just <$> mkCsvWriter a)
        Nothing -> pure Nothing
    modify
      (\x ->
         let ee = envExt x
         in x {ext = ee {csvWriter = csv}} :: SimEnv)
    writeCsvHeader


runStep :: SimEnv -> SimM () -> IO (Either TypeCheckErrors SimEnv)
runStep env act = do
  (res, env') <- runReprM env (runReaderT (unSimM act) mkContext)
  return $
    case res of
      Right () -> Right env'
      Left l   -> Left l

procStep :: SimEnv -> IO (Either TypeCheckErrors SimEnv)
procStep env =
  let act = mapM_ (modifyIORefM runProcess) =<< gets (simProcs . envExt)
  in runStep env act

writeCsvHeader ::  SimM ()
writeCsvHeader = do
  buses <- gets (simBuses . envExt)
  heads <-
    sort <$>
    concatForM
      buses
      (\BusInst {chans = chans, busInstName = bn}
          -- TODO: Add a lookupGlobalRef function to avoid this
        ->
         return $
         map
           (\x -> intercalate "_" $ map toString $ N.toList $ bn <> refOf x)
           (MM.keys chans))
  gets (csvWriter . envExt) >>= \case
    Just a -> liftIO $ writeCsvLine a heads
    Nothing -> return ()

busStep :: SimEnv -> IO (Either TypeCheckErrors SimEnv)
busStep env =
  runStep env $ do
    res <- concatMapM propagateBus =<< gets (simBuses . envExt)
    gets (csvWriter . envExt) >>= \case
      Just a -> liftIO $ writeCsvLine a res
      Nothing -> return ()

finalizeSim :: SimEnv -> IO (Either TypeCheckErrors SimEnv)
finalizeSim env =
  runStep env $ do
    applyTypes
    gets (csvWriter . envExt) >>= \case
      Just a -> liftIO $ finalizeCsv a
      Nothing -> return ()

toSimEnv :: Env -> SimEnv
toSimEnv = (<$) EmptyExt

runSimM :: Env -> SimM a -> IO (Either TypeCheckErrors a, SimEnv)
runSimM env act = runReprM (toSimEnv env) (runReaderT (unSimM act) mkContext)


simulate :: Int -> Env -> IO (Either TypeCheckErrors SimEnv)
simulate its e = do
  (res, env) <-
    runSimM e $ do
      setupSimEnv Nothing
      procs <- gets (simProcs . envExt)
      buses <- gets (simBuses . envExt)
      replicateM_ its (runSimulation procs buses)
      applyTypes
      gets (csvWriter . envExt) >>= \case
        Just a -> liftIO $ finalizeCsv a
        Nothing -> return ()
  return $
    case res of
      Right () -> Right env
      Left l   -> Left l

-- | Extract the maximum observed bus and variable values and use them to set
-- updated types for in the program
applyTypes :: SimM ()
applyTypes = do
  procs' <- gets (simProcs . envExt)
  procs <- liftIO $ mapM readIORef procs'
  buses <- gets (simBuses . envExt)
  updateVarTypes procs
  updateBusTypes buses
  return ()


chanValList :: [BusInst] -> IO [((Ref, Ident), Value)]
chanValList b =
  maxInGroup . sortBy (\(x, _) (y, _) -> compare x y) <$> concatMapM go b
  where
    go BusInst {..} =
      let vals = MM.elems chans
      in map (\(n, v) -> ((ref, n), v)) <$> mapM chanMaxVal vals


-- TODO: Replace by groupBy/maximumBY
maxInGroup :: (Eq a, Ord b) => [(a, b)] -> [(a, b)]
maxInGroup []         = []
maxInGroup [i]        = [i]
maxInGroup (i1:i2:is) = go i1 (i2 : is)
  where
    go c@(n1, v1) (e@(n2, v2):es)
      | n1 /= n2  = c : go e es
      | v2 > v1   = go e es
      | otherwise = go c es
    go c [] = [c]


-- | Return the maximum value logged for a bus
chanMaxVal :: BusChan -> IO (Ident, Value)
chanMaxVal LocalChan {..}    = (name, ) <$> readIORef maxBusVal
chanMaxVal ExternalChan {..} = (name, ) <$> readIORef maxBusVal


updateBusTypes :: [BusInst] -> SimM ()
updateBusTypes bs = do
  noStrict <- getConfig noStrictSizeBounds
  maxvals <- liftIO $ chanValList bs
  maxtypes <-
    forM
      maxvals
      (\((r, i), v) -> do
         t <- withScope (N.head r) $ lookupTy (r <> refOf i)
         rn' <- getRangeLit v
         let (t', rn'') =
               if noStrict || isUnsized t
                 then (valueToType v t, rn')
                 else (t, Nothing)
         return ((r, i), (t', rn'')))
  forM_
    maxtypes
    (\((r, i), (t, rn)) ->
       withScope (N.head r) $ updateDef r (setBusChanType i rn t))

getRangeLit :: Value -> SimM (Maybe Value)
getRangeLit v =
  getConfig noRangeAnnot >>= \case
    False -> pure (Just v)
    True -> pure Nothing


varValList :: [ProcInst] -> [(Ref, Value)]
varValList ps =
  maxInGroup . sortBy (\(x, _) (y, _) -> compare x y) $ concatMap go ps
  where
    go ProcInst {..} =
      mapMaybe (uncurry (simRefMaxVal fromEnt)) (M.toList valueTab)
    simRefMaxVal r i MutVal {maxVal = maxVal} =
      Just (refOf r <> refOf i, maxVal)
    simRefMaxVal _ _ _ = Nothing


updateVarTypes :: [ProcInst] -> SimM ()
updateVarTypes ps = do
  noStrict <- getConfig noStrictSizeBounds
  let maxvals = varValList ps
  maxtypes <-
    forM
      maxvals
      (\(r, v) -> do
         t <- withScope (N.head r) $ lookupTy r
         r' <- getRangeLit v
         let t' =
               if noStrict || isUnsized t
                 then (valueToType v t, r')
                 else (t, r')
         return (r, t'))
  forM_ maxtypes $ \(r, (t, r')) ->
    withScope (N.head r) $ updateDef r (setType t r')



-- We initialize a entity content by creating a value table initialized with
-- default values defined in the syntax. TODO: Per definition, default values
-- for expressions may be any static expression. Currently, the type checker
-- will only accept numeric literals as default value initializers. This should
  -- be fixed by having the type checker recursively look up expressions. For the
-- type checker, the expression types should be propagated back, while in the
-- simulator, static expressions should be evaluated. The type checker should
-- make sure that expressions that are passed on are actually static. This could
-- be done by building a dependency graph of the declarative part of each
-- process deciding the order that definitions should be instantiated in. Value
-- tables for network declarations are transient since network tables are not
-- present in the runtime representation. Value tables for processes, on the
-- other hand, are permanent as they exist for the duration of the process. The
-- entity instantiation functions are initially handed a value table where

-- addInst :: Ident -> ProcInst -> SimM ()
-- addInst i inst = do
--   traceM $ "Adding to inst: " ++ show i
--   updateCurEnv
--     (\d ->
--        let e = topExt d
--            e' = e {symTable = M.insert i (InstVal inst) (symTable e)}
--        in d {ext = e'})

-- getInst :: Ident -> SimM (Maybe ProcInst)
-- getInst i = do
--   e <- getCurEnv
--   return $ M.lookup i (instTab (topExt e))
--mkPhantomInstnace :: Ident -> SimM


-- instantiateNetwork :: Ident -> SimM ()
-- instantiateNetwork i = do
--   lookupTopDef i >>= \case
--     t@NetworkTable{netName = netName} ->
--       withScope netName $ instNetwork t
--     t@ProcessTable{} -> instProcess t


  -- (e, env) <-runSimM e setupSimEnv
  -- case e of


-- constructGraph :: SimM ()
-- constructGraph = do
--   edges' <- concat <$> mapUsedTopDefsM go
--   let nodes' = concatMap (\(e1, e2) -> [e1, e2]) edges'
--       edges'' = map (\((n1, s1), (n2, s2)) -> (n1, n2, s1 ++ "_" ++ s2)) edges'
--       graph = NetGraph $ mkGraph nodes' edges''
--   liftIO $ prettyPrint (unInstGraph graph)
  --     --go :: SimM (NodeMapM String String
--   where
--     go :: TopDef -> SimM [(LNode String, LNode String)]
--     go ProcessTable {..} = return []
--     go NetworkTable {netName = netName, symTable = symTable} =
--       concat <$> withScope netName (mapM go' (M.elems symTable))
--       where
--         go' InstDef { params = params
--                     , instName = thisInstName
--                     , ext = InstExt {nodeId = thisNodeId}
--                     } = do
--           siblings <-
--             forM
--               params
--               (\x -> do
--                  def <- lookupDef x
--                  traceM ("LookupDef in Simulate returned " ++ show def)
--                  return
--                    ( nodeId ((ext :: DefType -> SimExt) def)
--                    , toString ((instName :: DefType -> Ident) def)))
--           return $ map (\y -> ((thisNodeId, toString thisInstName), y)) siblings
--         go' _ = return []

-- --makeProcInst :: SimM ()

--data Instance

-- notes:

-- TODO (maybe): Parameterize the Name type of the SAT such that names can be
-- changed to a type which holds values.
-- Algorithm:
-- 1) Build execution environments for every process holding the value of local
-- variables
-- 2) Connect all buses to value references
-- 3) Connect all processes to a clock signal
-- 4) Run all process instances in parallel together with a process for running
-- the clock
-- 5) Log a trace of the values of every bus.
-- 6) How can we communicate with external processes in this manner?

-- Have a broadcasting channel for clock. All processes awaits a clock signal on
-- this channels. For communicating completed computation back to the execution
-- manager use a TMVar and set it to the ID of the finished process. When all
-- processes have reported in, swap the buses and tick the clock.

-- Prototype: Simulate simple self-generating networks

-- An instance should contain a reference to a bus instance linked to that
-- instance. When another process links to that bus, we should look up and link
-- to its instance and not the actual definition

-- Evaluate every top-level network definition. Create instances of processes
-- for every instance declaration. Keep track of instances in a map such that we
-- can resolve an instance name through a top-level reference. This is used for
-- resolving references to default instances of top-level processes. It may not
-- be so simple: for every instantiated process, either look up the instance and
-- create a reference to existing buses or create a new bus definition.

-- TODO: Using buses declared in processes with multiple instances by referring
-- to the bus through the process name is ambiguous (unless the bus is
-- unique). Keep track of this in the type checker by logging which tracking
-- when a bus is instantiated. A simple rule here could be that if a bus is
-- instnatiated with a name, it's buses must be referenced through the instance
-- name (i.e. passed as a parameter) when used as input for another
-- bus. Alternatively, (or complimentary) we build a graph of instnaces which
-- will make it easy to spot irresolvable ambiguous connections.

-- Connection map building algorithm:
-- 1) When an instance declaration is encountered, create an instance for that
-- process and also instantiate all buses that it uses
-- 2) Assumption here: When we instantiate a bus, we know the context that it is
-- instantiated in so the process types of other instance are easily accessible.
-- 3) ..
--
-- 1) When encountering a process
---- 2)
