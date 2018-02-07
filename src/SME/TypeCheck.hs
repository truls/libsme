{-# LANGUAGE DuplicateRecordFields      #-}
-- {-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}

module SME.TypeCheck
  ( typeCheck
  ) where

--import           Control.Lens
import           Control.Exception           (Exception, SomeException, throw)
import           Control.Monad.Except
import           Control.Monad.State         (MonadState, State, execState,
                                              forM, forM_, gets, modify,
                                              runState, when)
import           Data.Generics.Uniplate.Data (universeBi)
import qualified Data.HashMap.Strict         as M

import           Language.SMEIL.Syntax
import           SME.Error

-- Datatypes for holding symbol tables and the objects they resolve to

-- | Main typechecking monad
newtype TyM a = TyM
  { unTyM :: ExceptT TypeCheckErrors (State Env) a
  } deriving (Functor, Applicative, Monad, MonadState Env)

-- | Datatype for holding the typechecking environment
data Env = Env
  { procs   :: M.HashMap String ProcessTable
  , nets    :: M.HashMap String ProcessTable
  , curProc :: Maybe ProcessTable
  , curNet  :: Maybe NetworkTable
  } deriving (Show)

mkEnv :: Env
mkEnv =
  Env {procs = M.empty, nets = M.empty, curProc = Nothing, curNet = Nothing}

data DefType
  = VarDef { varName :: Ident
           , varDef  :: Variable }
  | ConstDef { constName :: Ident
             , constDef  :: Constant }
  | BusDef { busName :: Ident
           , busDef  :: Bus }
  | FunDef { funcName :: Ident
           , funcDef  :: Function }
  | EnumMap { enumName :: Ident
            , enumDef  :: Enumeration }
  | InstDef { instName :: Ident
            , instDef  :: Instance }
  deriving (Show)


data ProcessTable = ProcessTable
  --   varMap   :: M.Map String Variable
  -- , constMap :: M.Map String Constant
  -- , busMap   :: M.Map String Bus
  -- , funcMap  :: M.Map String Function
  -- , enumMap  :: M.Map String Enumeration
  -- , instMap  :: M.Map String Instance
  { symTable :: M.HashMap String DefType
  , pro      :: Process
  } deriving (Show)

mkProcessMap :: Process -> ProcessTable
mkProcessMap = ProcessMap M.empty

data NetworkMap = NetworkMap
  --   instMap  :: M.Map String Instance
  -- , busMap   :: M.Map String Bus
  -- , constMap :: M.Map String Constant
  { symTable :: M.HashMap String DefType
  , net      :: Network
  } deriving (Show)

mkNetworkMap :: Network -> NetworkMap
mkNetworkMap = NetworkMap M.empty

lookupTy :: (References a) => a -> TyM Type
lookupTy = undefined

unifyTypes :: Type -> Type -> TyM Type
unifyTypes = undefined

-- | Check expression and update references as needed
checkExpr :: Expr -> TyM (Type, Expr)
checkExpr p@PrimName {..} = (,p) <$> lookupTy name
checkExpr p@FunCall {..}  = (,p) <$> lookupTy name
checkExpr p@PrimLit {..} =
  case typeOf lit of
    Typed t -> return (t, p)
    Untyped -> error "foo"
checkExpr Binary {..}   = do
  (t1, e1) <- checkExpr left
  (t2, e2) <- checkExpr right
  t' <- unifyTypes t1 t2
  return (t', Binary binOp e1 e2 loc)
checkExpr Unary {..} = do
  (t', e') <- checkExpr expr
  return (t', Unary unOp e' loc)

matchType :: (Typed t) => t -> t
matchType = undefined

-- addNetwork :: Network -> TyM NetworkMap
-- addNetwork

enterProc :: Process -> TyM () -> TyM ()
enterProc p m = do
  let n = (name :: Process -> Ident) p
  st <- gets curProc
  case st of
    Nothing ->
      modify (\s -> s { curProc = Just $ mkProcessMap p } )
    Just _ -> error "Compiler bug"
  m

-- procState :: Declaration -> TyM ()
-- procState (VarDecl (v@Variable{..})) =

-- addToProcMap :: String -> DefType -> TyM ()
-- addToProcMap n dt = do
--   st <- gets curProc >>= \case
--     Just a -> return a
--     Nothing -> error "Compiler Bug"

-- addProcMap :: ProcessMap -> TyM ()
-- addProcMap p = do
--   st <- gets procs
--   case M.lookup (toString na) st of
--     Just p' -> throw $ DuplicateName na (name (pro p' :: Process))
--     Nothing ->
--       modify $ \s ->
--         s {procs = M.insert (toString na) (mkProcessMap p) (procs s)}

-- | Adds a process to the environment
addProc :: Process -> TyM ()
addProc p@Process {name = na} = do
  pc <- gets procs
  ns <- gets nets
  case M.lookup (toString na) ns of
    Just p' -> throw $ DuplicateName na (name (net p' :: Network))
    Nothing ->
      case M.lookup (toString na) pc of
        Just p' -> throw $ DuplicateName na (name (pro p' :: Process))
        Nothing ->
          modify $ \s ->
            s {procs = M.insert (toString na) (mkProcessMap p) (procs s)}

-- | Adds a network to the environment
addNet :: Network -> TyM ()
addNet n@Network {name = netName} = do
  ns <- gets nets
  pc <- gets procs
  case M.lookup (toString netName) pc of
    Just a -> throw $ DuplicateName netName (name (pro a :: Process))
    Nothing ->
      case M.lookup (toString netName) ns of
        Just p -> throw $ DuplicateName netName (name (net p :: Network))
        Nothing ->
          modify $ \s ->
            s {nets = M.insert (toString netName) (mkNetworkMap n) (nets s)}

-- | Initially populate typechecking environment with all functions defined.
initialEnv :: DesignFile -> TyM ()
initialEnv df =
  forM_ (universeBi df :: [Network]) addNet >>
  forM_ (universeBi df :: [Process]) addProc

-- | Do typechecking of an environment. Return DesignFile with completed type
-- annoations
typeCheck :: DesignFile -> IO DesignFile
typeCheck df = do
  let s = execState (runExceptT $ unTyM (initialEnv df)) mkEnv
  print s
  return df
