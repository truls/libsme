{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Reconstructs SMEIL source code from an environment

module SME.Reconstruct (reconstruct) where

import qualified Data.HashMap.Strict   as M
import           Data.Loc              (noLoc)
import           Data.Maybe            (mapMaybe)

import           Language.SMEIL.Syntax
import           SME.Representation

type Env = BaseEnv Void
type TopDef = BaseTopDef Void
type DefType = BaseDefType Void

reBusDef :: Bool -> Bus -> Ident -> BusShape -> Bus
reBusDef isExposed busDef busName busShape =
  Bus isExposed (unique busDef) busName (map mkChan (unBusShape busShape)) noLoc
  where
    mkChan (n, (ty, lit)) =
      BusSignal n ty ((\x -> PrimLit ty x noLoc) <$> lit) Nothing noLoc

reDefTypeNet :: [DefType] -> [NetworkDecl]
reDefTypeNet = mapMaybe go
  where
    go ConstDef {..} = Just $ NetConst constDef
    go BusDef {..} = Just $ NetBus $ reBusDef isExposed busDef busName busShape
    go InstDef {..} = Just $ NetInst instDef
    go _ = Nothing

reDefTypeProc :: [DefType] -> [Declaration]
reDefTypeProc = mapMaybe go
  where
    go ConstDef {..} = Just $ ConstDecl constDef
    go VarDef {..} = Just $ VarDecl varDef
    go BusDef {..} = Just $ BusDecl $ reBusDef isExposed busDef busName busShape
    go InstDef {..} = Just $ InstDecl instDef
    go _ = Nothing

reTopDef :: TopDef -> UnitElement
-- TODO: Params
reTopDef NetworkTable {..} =
  UnitNet $ Network netName [] (reDefTypeNet (M.elems symTable)) noLoc
reTopDef ProcessTable { procName = pn
                      , symTable = st
                      , stms = stms
                      , procDef = procDef
                      } =
  UnitProc
    Process
    { name = pn
    , params = (params :: Process -> [Param]) procDef
    , decls = reDefTypeProc (M.elems st)
    , body = stms
    , sync = sync procDef
    , loc = noLoc
    }

reconstruct :: Env -> DesignFile
reconstruct BaseEnv {..} =
  DesignFile [DesignUnit [] (map reTopDef (M.elems defs)) noLoc] noLoc
