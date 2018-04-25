{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module SME.API
  (
  ) where

import           Control.Exception     (try, tryJust)
import           Foreign               (Ptr)
import           Foreign.C.String      (CString, peekCString, withCString)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.StablePtr     (StablePtr, deRefStablePtr,
                                        freeStablePtr, newStablePtr)

import qualified Data.Text.IO          as T
import           Options.Applicative   (ParserResult (..), defaultPrefs,
                                        execParserPure, info)

import           Text.Show.Pretty      (ppShow)

import           Language.SMEIL.Pretty
import           SME.API.Internal
import           SME.CodeGen
import           SME.Error
import           SME.ImportResolver
import           SME.Reconstruct
import           SME.Representation
import           SME.Simulate
import           SME.Stages
import           SME.TypeCheck

import           Debug.Trace

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
foreign import ccall "sme_get_sim_state"
  sme_get_sim_state :: SmeCtxPtr -> IO SimCtxPtr
foreign import ccall "sme_set_error"
  sme_set_error :: SmeCtxPtr -> CString -> IO ()

data ApiCtx = ApiCtx
  { libCtx        :: SmeCtxPtr
  , tyState       :: Env
  , simState      :: SimEnv
  , apiConf       :: Config
  , errorRenderer :: TypeCheckErrors -> String
  }

setError :: SmeCtxPtr -> String -> IO ()
setError ptr e = withCString e (sme_set_error ptr)

loadFile :: SmeCtxPtr -> CString -> Int -> CStringArray -> IO Bool
loadFile c f n arr = do
  args <- mapM peekCString =<< peekArray n arr
  case execParserPure defaultPrefs (info libOptParser mempty) args of
    CompletionInvoked _ -> return False
    Failure fa -> do
      setError c (show fa)
      return False
    Success a -> go a
  where
    go conf = do
      file <- peekCString f
      -- TODO: Catch IO errors here
      try (resolveImports file) >>= \case
        Left e -> do
          setError c (show (e :: ImportingError)) --(renderError nameMap e)
          return False
        Right (df, nameMap) -> do
          res <- try (typeCheck df conf)
          case res of
            Left e ->
              trace "first res left" $
              setError c (renderError nameMap (e :: TypeCheckErrors)) >>
              return False
            Right r ->
              trace "first res right" $
              newSteppingSim r c >>= \case
                Left e ->
                  trace "second res lefft" $
                  setError c (renderError nameMap e) >> return False
                Right r' ->
                  trace "second res right" $ do
                    ptr <-
                      newStablePtr
                        ApiCtx
                        { libCtx = c
                        , simState = r'
                        , tyState = Void <$ r'
                        , errorRenderer = renderError nameMap
                        , apiConf = conf
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
  -- TODO: If simulation was run, make sure that finalize was also run before
  -- genCode is called.
  ctx <- deRefStablePtr c
  freeStablePtr c
  s' <- peekCString n
  T.putStrLn (pprr (reconstruct (tyState ctx)))
  --putStrLn $ ppShow (reconstruct (tyState ctx)) --(tyState ctx)
  try (genOutput s' VHDL (tyState ctx)) >>= \case
    Left e -> do
      setError (libCtx ctx) (show (e :: TypeCheckErrors))
      return False
    Right _ -> return True

propagateBuses :: SimCtxPtr -> IO Bool
propagateBuses c = runStep c busStep

runProcs :: SimCtxPtr -> IO Bool
runProcs c = runStep c procStep

-- Duplicating this here is a bit hackish
runTypeCheck :: SimCtxPtr -> IO Bool
runTypeCheck c = do
  ctx <- deRefStablePtr c
  freeStablePtr c
  res <- try (typeCheck (reconstruct (Void <$ simState ctx)) (apiConf ctx))
  case res of
    Left e -> do
      setError (libCtx ctx) (errorRenderer ctx (e :: TypeCheckErrors))
      return False
    Right r -> do
      sme_set_sim_state (libCtx ctx) =<< newStablePtr ctx {tyState = r}
      return True

finalize :: SimCtxPtr -> IO Bool
finalize c
  -- This is also _very_ hackish
 = do
  smeCtx <- libCtx <$> deRefStablePtr c
  -- StablePtr is freed by call to runStep
  res <- runStep c finalizeSim
  if res
    then do
      ctx <- sme_get_sim_state smeCtx
      runTypeCheck ctx
    else return res
