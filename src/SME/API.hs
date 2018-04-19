{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module SME.API
  (
  ) where

import           Control.Exception  (try, tryJust)
import           Foreign.C.String   (CString, peekCString, withCString)
import           Foreign.StablePtr  (StablePtr, deRefStablePtr, freeStablePtr,
                                     newStablePtr)

import           SME.API.Internal
import           SME.CodeGen
import           SME.Error
import           SME.ImportResolver
import           SME.Representation
import           SME.Simulate
import           SME.Stages
import           SME.TypeCheck

--import           Debug.Trace

trace :: String -> a -> a
trace _ = id

-- traceM :: (Applicative f) => String -> f ()
-- traceM _ = pure ()

type SimCtxPtr = StablePtr ApiCtx

foreign export ccall "hs_propagate_buses"
  propagateBuses :: SimCtxPtr -> IO Bool
foreign export ccall "hs_run_procs"
  runProcs :: SimCtxPtr -> IO Bool
foreign export ccall "hs_sme_load_file"
  loadFile :: SmeCtxPtr -> CString -> IO Bool
foreign export ccall "hs_finalize"
  finalize :: SimCtxPtr -> IO Bool
foreign export ccall "hs_gen_code"
  genCode :: SimCtxPtr -> CString -> IO Bool

foreign import ccall "sme_set_sim_state"
  sme_set_sim_state :: SmeCtxPtr -> SimCtxPtr -> IO ()
foreign import ccall "sme_set_error"
  sme_set_error :: SmeCtxPtr -> CString -> IO ()

data ApiCtx = ApiCtx
  { libCtx        :: SmeCtxPtr
  , config        :: Config
  , simState      :: SimEnv
  , errorRenderer :: TypeCheckErrors -> String
  }

setError :: SmeCtxPtr -> String -> IO ()
setError ptr e = withCString e (sme_set_error ptr)

loadFile :: SmeCtxPtr -> CString -> IO Bool
loadFile c f = do
  file <- peekCString f
  -- TODO: Catch IO errors here
  try (resolveImports file) >>= \case
    Left e -> do
      setError c (show (e :: ImportingError)) --(renderError nameMap e)
      return False
    Right (df, nameMap) -> do
      res <- try (typeCheck df)
      case res of
        Left e ->
          trace "first res left" $
          setError c (renderError nameMap (e :: TypeCheckErrors)) >> return False
        Right r ->
          trace "first res right" $
          newSteppingSim r c >>= \case
            Left e ->
              trace "second res left" $
              setError c (renderError nameMap e) >> return False
            Right r' ->
              trace "second res right" $ do
                ptr <-
                  newStablePtr
                    ApiCtx
                    { libCtx = c
                    , config = mkConfig
                    , simState = r'
                    , errorRenderer = renderError nameMap
                    }
                sme_set_sim_state c ptr
                return True

runStep ::
     SimCtxPtr -> (SimEnv -> IO (Either TypeCheckErrors SimEnv)) -> IO Bool
runStep c f = do
  ctx <- deRefStablePtr c
  freeStablePtr c
  res <-
    tryJust
      (\(e :: TypeCheckErrors) -> Just $ errorRenderer ctx e)
      (f (simState ctx))
  case res of
    Right s ->
      case s of
        Right s' ->
          trace "runStep right" $ do
            sme_set_sim_state (libCtx ctx) =<< newStablePtr ctx {simState = s'}
            return True
        Left e -> setError (libCtx ctx) (errorRenderer ctx e) >> return False
    Left e ->
      trace "runStep left" $ do
        setError (libCtx ctx) e
        return False

genCode :: SimCtxPtr -> CString -> IO Bool
genCode c n = do
  ctx <- deRefStablePtr c
  freeStablePtr c
  s' <- peekCString n
  try (genOutput s' VHDL (Void <$ simState ctx)) >>= \case
    Left e -> do
      setError (libCtx ctx) (show (e :: TypeCheckErrors))
      return False
    Right _ -> return True


propagateBuses :: SimCtxPtr -> IO Bool
propagateBuses c = runStep c busStep

runProcs :: SimCtxPtr -> IO Bool
runProcs c = runStep c procStep

finalize :: SimCtxPtr -> IO Bool
finalize c = runStep c finalizeSim
