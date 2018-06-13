{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module SME.API
  (
  ) where

import           Control.Exception      (try)
import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (local)
import           Control.Monad.State    (MonadState (put), StateT, gets, lift,
                                         runStateT)
import           Foreign                (Ptr)
import           Foreign.C.String       (CString, peekCString, withCString)
import           Foreign.Marshal.Array  (peekArray)
import           Foreign.StablePtr      (StablePtr, deRefStablePtr,
                                         freeStablePtr, newStablePtr)

import           Options.Applicative    (ParserResult (..), defaultPrefs,
                                         execParserPure, info)

import           SME.API.Internal
import           SME.Error
import           SME.Representation
import           SME.Simulate
import           SME.Stages

--import           Debug.Trace

-- trace :: String -> a -> a
-- trace _ = id

-- traceM :: (Applicative f) => String -> f ()
-- traceM _ = pure ()

type Env = BaseEnv Void

type CStringArray = Ptr CString

type SimCtxPtr = StablePtr ApiCtx

foreign export ccall "hs_propagate_buses"
  propagateBuses :: SimCtxPtr -> IO Bool
foreign export ccall "hs_run_procs"
  runProcs :: SimCtxPtr -> IO Bool
foreign export ccall "hs_sme_load_file"
  loadFile :: SmeCtxPtr -> CString -> Int -> CStringArray -> IO Bool
foreign export ccall "hs_finalize"
  finalize :: SimCtxPtr -> IO Bool
foreign export ccall "hs_gen_code"
  genCode :: SimCtxPtr -> CString -> IO Bool

foreign import ccall "sme_set_sim_state"
  sme_set_sim_state :: SmeCtxPtr -> SimCtxPtr -> IO ()
-- foreign import ccall "sme_get_sim_state"
--   sme_get_sim_state :: SmeCtxPtr -> IO SimCtxPtr
foreign import ccall "sme_set_error"
  sme_set_error :: SmeCtxPtr -> CString -> IO ()

data ApiCtx = ApiCtx
  { libCtx        :: SmeCtxPtr
  , tyState       :: Env
  , simState      :: SimEnv
  , apiConf       :: Config
  , compilerState :: CompilerState
  }

mkApiCtx :: SmeCtxPtr -> Config -> ApiCtx
mkApiCtx ptr cfg =
  ApiCtx
  { libCtx = ptr
  , tyState = mkEnv mkConfig Void
  , simState = mkEnv mkConfig emptyExt
  , apiConf = cfg
  , compilerState = mkCompilerState
  }

newtype ApiM a = ApiM
  { unApiM :: StateT ApiCtx CompilerM a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState ApiCtx)

type ActRet a = Either () (a, ApiCtx)

runApiM :: ApiCtx -> ApiM a -> IO (ActRet a)
runApiM api@ApiCtx {apiConf = cfg, compilerState = cs} f
 = do
  res <- try $ runCompilerM cfg cs (handleErrors $ runStateT (unApiM f) api)
  case res of
    Right ((v, api'), cs')    -> return $ Right (v, api' {compilerState = cs'})
    Left (e :: SomeException) -> do setError (libCtx api) (show e)
                                    return $ Left ()

liftApiM :: CompilerM a -> ApiM a
liftApiM = ApiM . lift

setError :: SmeCtxPtr -> String -> IO ()
setError ptr e = withCString e (sme_set_error ptr)


loadPipe :: FilePath -> ApiM (BaseEnv Void)
loadPipe fp =
  liftApiM $
  (doImports >=>
   dumpStage ResolveImport >=>
   doTypeCheck >=>
   dumpStage ResolveImport >=> doTransform >=> dumpStage Transform)
    fp

postSimPipe :: ApiCtx -> SimEnv -> ApiM SimEnv
postSimPipe ctx e = do
  res <-
    liftApiM $
    (doReconstruct >=>
     dumpStage Retyped >=>
     writeRetyped >=>
     doTypeCheck >=>
     dumpStage RetypeCheck)
      (Void <$ e)
  -- FIXME: This is a hack which can be eliminated by separating network
  -- analysis from the simulator
  liftIO $ initSimEnv res (libCtx ctx)

genCodePipe :: FilePath -> ApiM ()
genCodePipe fp = do
  st <- gets tyState
  liftApiM $ local (\x -> x {outputDir = Just fp}) (doOutput st)

initSim :: SmeCtxPtr -> Env -> ApiM SimEnv
initSim p e = liftIO $ newSteppingSim p e

runSimStep :: (SimEnv -> IO SimEnv) -> ApiM SimEnv
runSimStep act = do
  ss <- gets simState
  liftIO $ act ss

runSimFinalize, runSimProcs, runSimBuses :: ApiM SimEnv
runSimFinalize = runSimStep finalizeSim
runSimProcs = runSimStep procStep
runSimBuses = runSimStep busStep

apiAction :: SimCtxPtr -> (ApiCtx -> a -> ApiCtx) -> ApiM a -> IO Bool
apiAction c f act = do
  ctx <- deRefStablePtr c
  freeStablePtr c
  runApiM ctx act >>= finishApiRet f

apiAction' ::
     SimCtxPtr -> (ApiCtx -> a -> ApiCtx) -> (ApiCtx -> ApiM a) -> IO Bool
apiAction' c f act = do
  ctx <- deRefStablePtr c
  freeStablePtr c
  runApiM ctx (act ctx) >>= finishApiRet f

finishApiRet :: (ApiCtx -> a -> ApiCtx) -> ActRet a ->  IO Bool
finishApiRet _ (Left ()) = return False
finishApiRet f (Right (r, ctx)) = do
  let ctx' = f ctx r
  ptr' <- newStablePtr ctx'
  sme_set_sim_state (libCtx ctx') ptr'
  return True

loadFile :: SmeCtxPtr -> CString -> Int -> CStringArray -> IO Bool
loadFile c f n arr = do
  args <- mapM peekCString =<< peekArray n arr
  case execParserPure defaultPrefs (info libOptParser mempty) args of
    CompletionInvoked _ -> return False
    Failure fa -> do
      setError c (show fa)
      return False
    Success cfg ->
      peekCString f >>=
      (\fp ->
         runApiM
           (mkApiCtx c cfg)
           (do e <- loadPipe fp
               se <- initSim c e
               put
                 ApiCtx
                 { libCtx = c
                 , tyState = Void <$ se
                 , simState = se
                 , apiConf = cfg
                 , compilerState = mkCompilerState
                 }) >>=
         finishApiRet const)

genCode :: SimCtxPtr -> CString -> IO Bool
genCode c n = do
  n' <- peekCString n
  apiAction c const (genCodePipe n')

simAction :: ApiM SimEnv -> SimCtxPtr -> IO Bool
simAction act c = apiAction c (\x e -> x {simState = e}) act

propagateBuses, runProcs :: SimCtxPtr -> IO Bool
propagateBuses = simAction runSimBuses
runProcs = simAction runSimProcs

finalize :: SimCtxPtr -> IO Bool
finalize c =
  apiAction'
    c
    (\x e -> x {tyState = Void <$ e})
    (\x -> runSimFinalize >>= postSimPipe x)
