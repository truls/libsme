{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

-- | SME network simulator

module SME.Simulate (simulate) where

import           Control.Exception                 (throw)
import           Control.Monad                     (forM, mapM_, void)
import           Control.Monad.Error               (MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.State               (MonadState, StateT, get,
                                                    modify, runStateT)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.HashMap.Strict               as M
import           Data.IORef
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as N

import           Language.SMEIL.Syntax             hiding (instName)
import           SME.Error
import           SME.Representation

import           Debug.Trace

-- import SME.APITypes

type Env = BaseEnv Void
type DefType = BaseDefType SimExt
type TopDef = BaseTopDef SimExt

newtype SimM a = GenM
  { unSimM :: ReprM IO SimExt a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState SimEnv
             , MonadError TypeCheckErrors
             , MonadIO
             )

instance (MonadRepr SimExt) SimM
  --lookupDef' :: (References a) => a -> m ([Ident], BaseDefType s)
                                                                    where
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


data Value = Value
  deriving Show

newtype NetGraph = NetGraph {unNetGraph :: Gr String String}
-- unNetGraph :: NetGraph -> (Gr String String)
-- unNetGraph (NetGraph g) = g
-- Should be:
--type NetGraph = Gr ProcInst (IORef BusInst)

data SimExt
  = EnvExt { labelSource :: Int }
           --, netGraph    ::  NetGraph}
  | ProcTabExt
  | NetTabExt { valueTab :: M.HashMap String Value }
  | VarExt
  | ConstExt { value :: Value }
  | BusExt
  | FunExt
  | EnumExt
  | EnumFieldExt
  | InstExt { nodeId :: Int }
  | ParamExt
  | EmptyExt
  deriving (Show)


instance Extension SimExt where
  emptyExt = EmptyExt


data BusChan = BusChan
  { name   :: Ident
  , maxVal :: IORef Value
  , read   :: IORef Value
  , write  :: IORef Value
  }

data BusInst = BusInst
  { chans :: [BusChan]
  , ref   :: Ref -- ^Reference to the bus that this was instantiated from
  }


data ProcInst = ProcInst
  { params   :: [(Ref, Param)]
  , valueTab :: IORef (M.HashMap String Value)
  , stmts    :: [Statement]
  }

type RunM = StateT ProcInst IO

lookupValue :: Ref -> ProcInst -> Value
lookupValue = undefined

runProcess :: ProcInst -> IO ()
runProcess p@ProcInst {stmts = stmts} =
  void $ runStateT (mapM_ interpStm stmts) p

interpStm :: Statement -> RunM ()
interpStm Assign {..} = undefined

interpExpr :: Expr -> RunM ()
interpExpr = undefined

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
labelInstances = forUsedTopDefsM_ go
  where
    go ProcessTable {..} = do
      symTab' <- mapM go' symTable
      updateTopDef procName (\x -> x {symTable = symTab'})
    go NetworkTable {..} = do
      symTab' <- mapM go' symTable
      updateTopDef netName (\x -> x {symTable = symTab'})
    go' i@InstDef {} = do
      newLab <- getFreshLabel
      return ((i {ext = InstExt {nodeId = newLab}}) :: DefType)
    go' i = return i

-- setupNetwork :: [DefType] -> SimM ()
-- setupNetwork = mapM_ go
--   where
--     go InstDef {..} =

--setupTopDefs ::

constructGraph :: SimM ()
constructGraph = do
  edges' <- concat <$> mapUsedTopDefsM go
  let nodes' = concatMap (\(e1, e2) -> [e1, e2]) edges'
      edges'' = map (\((n1, s1), (n2, s2)) -> (n1, n2, s1 ++ "_" ++ s2)) edges'
      graph = NetGraph $ mkGraph nodes' edges''
  liftIO $ prettyPrint (unNetGraph graph)
    --go :: SimM (NodeMapM String String
  where
    go :: TopDef -> SimM [(LNode String, LNode String)]
    go ProcessTable {..} = return []
    go NetworkTable {netName = netName, symTable = symTable} =
      concat <$> withScope netName (mapM go' (M.elems symTable))
      where
        go' InstDef { params = params
                    , instName = thisInstName
                    , ext = InstExt {nodeId = thisNodeId}
                    } = do
          siblings <-
            forM
              params
              (\x -> do
                 def <- lookupDef x
                 traceM ("LookupDef in Simulate returned " ++ show def)
                 return
                   ( nodeId ((ext :: DefType -> SimExt) def)
                   , toString ((instName :: DefType -> Ident) def)))
          return $ map (\y -> ((thisNodeId, toString thisInstName), y)) siblings
        go' _ = return []

setupSimEnv :: SimM ()
setupSimEnv = do
  modify (\x -> x {ext = EnvExt {labelSource = 0}} :: SimEnv)
  labelInstances
  constructGraph

--type SimEnv = BaseEnv
type SimEnv = BaseEnv SimExt

toSimEnv :: Env -> SimEnv
toSimEnv = (<$) EmptyExt


runSimM :: Env -> SimM a -> IO (Either TypeCheckErrors a, SimEnv)
runSimM env act = runReprM (toSimEnv env) (unSimM act)

simulate :: Env -> IO ()
simulate e =
  void $ runSimM e setupSimEnv

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

