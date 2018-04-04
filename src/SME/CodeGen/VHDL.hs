{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module SME.CodeGen.VHDL (genVHDL) where

import           Control.Exception     (throw)
import           Control.Monad         (forM, mapM)
import qualified Data.HashMap.Strict   as M
import           Data.List             (intercalate)
import qualified Data.List.NonEmpty    as N
import           Data.Maybe            (catMaybes, mapMaybe)
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Language.VHDL         as V
import           Language.VHDL.Quote   as Q

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.CodeGen.Common
import           SME.CodeGen.TH
import           SME.Error
import           SME.Representation

import           Debug.Trace           (trace)

csvUtil :: V.DesignFile
csvUtil = $(parseVHDLToQexp "rts/vhdl/csv_util.vhdl")

ctx :: [V.ContextItem]
ctx = [contextitems|library ieee;
                   use ieee.std_logic_1164.all;
                   use ieee.std_numeriic.all;|]

genType :: Typeness -> V.SubtypeIndication
genType (Typed (Unsigned Nothing _)) = [subtyind|integer|]
genType (Typed (Unsigned (Just 1) _)) = [subtyind|unsigned(1 downto 0)|]
genType (Typed (Unsigned (Just s) _)) =
  [subtyind|unsigned($lit:(s-1) downto 0)|]
genType (Typed (Signed Nothing _)) = [subtyind|integer|]
genType (Typed (Signed (Just s) _)) = [subtyind|signed($lit:(s-1) downto 0)|]
genType t@(Typed _) = genBuiltinType t
genType Untyped = [subtyind|untyped|]

-- | Generate VHDL builtin types such as integer instead of std_numeric type
genBuiltinType :: Typeness -> V.SubtypeIndication
genBuiltinType (Typed Signed {})   = [subtyind|integer|]
genBuiltinType (Typed Unsigned {}) = [subtyind|integer|]
genBuiltinType (Typed Bool {})     = [subtyind|boolean|]
genBuiltinType _                   = [subtyind|untyped|]

genName :: Name -> GenM V.Name
genName Name {..} =
  let n =
        intercalate "_" $
        map (\IdentName {ident = i} -> (toString i)) (base : parts)
  in return [Q.name|$ident:n|]

genLit :: Literal -> V.Expression
genLit LitInt {..} = [expr|$lit:intVal|]
genLit LitFloat {..} = [expr|$lit:floatVal|]
genLit LitString {..} = [expr|$lit:stringVal|]
genLit LitArray {..}  =
            let
              elAssocs = map (\x -> [elassoc|$lit:x|]) arrayVal
            in
              [expr|($elassocs:elAssocs)|]
genLit LitTrue {..} = [expr|true|]
genLit LitFalse {..} = [expr|false|]

-- | Generate cooresponding VHDL code for an expression
genExpr :: Expr -> GenM V.Expression
genExpr Binary {..} = do
  e1 <- genExpr left
  e2 <- genExpr right
  return $ genBinOp binOp e1 e2
  where
    genBinOp (PlusOp _) e1 e2  = [expr|$expr:e1 + $expr:e2|]
    genBinOp (MinusOp _) e1 e2 = [expr|$expr:e1 - $expr:e2|]
    genBinOp (MulOp _) e1 e2   = [expr|$expr:e1 + $expr:e2|]
    genBinOp (DivOp _) e1 e2   = [expr|$expr:e1 + $expr:e2|]
    genBinOp (ModOp _) e1 e2   = [expr|$expr:e1 mod $expr:e2|]
    genBinOp (EqOp _) e1 e2    = [expr|$expr:e1 = $expr:e2|]
    genBinOp (NeqOp _) e1 e2   = [expr|$expr:e1 /= $expr:e2|]
    genBinOp (SllOp _) e1 e2   = [expr|$expr:e1 sll $expr:e2|]
    genBinOp (SrlOp _) e1 e2   = [expr|$expr:e1 srl $expr:e2|]
    genBinOp (LtOp _) e1 e2    = [expr|$expr:e1 < $expr:e2|]
    genBinOp (GtOp _) e1 e2    = [expr|$expr:e1 > $expr:e2|]
    genBinOp (LeqOp _) e1 e2   = [expr|$expr:e1 <= $expr:e2|]
    genBinOp (GeqOp _) e1 e2   = [expr|$expr:e1 >= $expr:e2|]
    genBinOp (AndOp _) e1 e2   = [expr|$expr:e1 and $expr:e2|]
    genBinOp (OrOp _) e1 e2    = [expr|$expr:e1 or $expr:e2|]
    genBinOp (XorOp _) e1 e2   = [expr|$expr:e1 xor $expr:e2|]
    genBinOp (ConOp _) e1 e2   = [expr|$expr:e1 and $expr:e2|]
    genBinOp (DisOp _) e1 e2   = [expr|$expr:e1 or $expr:e2|]

genExpr Unary {..} = do
  e <- genExpr right
  return $ genUnOp unOp e
  where
    genUnOp (UnPlus _) e  = [expr|+$expr:e|]
    genUnOp (UnMinus _) e = [expr|-$expr:e|]
    genUnOp (NotOp _) e   = [expr|not $expr:e|]

genExpr PrimName {name = n} = do
  n' <- genName n
  return [expr|$name:(n')|]
   -- let
   -- name = foldl (\n m -> [Q.name|$name:n . $name:m|]
    --[Q.name|$name:(go base)|] parts
   -- in return [expr|$expr:name|]
   --   where
   --     go (IdentName i _)     = [Q.name|$name:(toString i)|]
   --     go (ArrayAccess n i _) = [Q.name|$name:(go n) ( $expr:i )|]
  -- let str = pprrString n in
  --                          return [expr|$ident:str|]
genExpr FunCall {name = n, params = params} = do
  assocEls <-
    mapM
      (\x -> do
         x' <- genExpr x
         return [assocel|$expr:(x')|])
      params
  let n' = pprrString n
  return [expr|$ident:(n') ($assocels:assocEls)|]
genExpr PrimLit {lit = l} =
  return $ genLit l
genStm :: Statement -> GenM V.SequentialStatement
genStm Assign {..} = do
  e <- genExpr val
  d <- genName dest
  -- return [seqstm|$ident:d <= $expr:e;|]
  lookupDef dest >>= \case
    BusDef {} -> return [seqstm|$name:d <= $expr:e;|]
    VarDef {} -> return [seqstm|$name:d := $expr:e;|]
    _ ->
      throw $
      CompilerError "Unassignable constructs should never occur at this point"
genStm If {..} = do
  c <- genExpr cond
  b <- mapM genStm body
  elifs <-
    forM elif (\(cond', stmts) -> (,) <$> genExpr cond' <*> mapM genStm stmts)
  els' <-
    case els of
      Just e  -> Just <$> mapM genStm e
      Nothing -> pure Nothing
  return $ V.SIf $ V.IfStatement Nothing (c, b) elifs els'
genStm Switch {..} = do
  val <- genExpr value
  c' <-
    forM
      cases
      (\(cond', stmts) -> do
         a <- genExpr cond'
         b <- mapM genStm stmts
         return [casealt|when $expr:a => $seqstms:b|])
  dc <-
    case defaultCase of
      Just s -> do
        s' <- mapM genStm s
        return [[casealt|when others => $seqstms:(s')|]]
      Nothing -> return []
  return [seqstm|case $expr:val is $casealts:(c' ++ dc) end case;|]
genStm For {var = var, body = body, from = from, to = to} = do
  body' <- mapM genStm body
  from' <- genExpr from
  to' <- genExpr to
  return [seqstm|for $ident:(toString var) in $expr:(from') to $expr:(to') loop
                $seqstms:(body')
                end loop;|]
genStm Return {..} =
  case retVal of
    Just e -> do
      e' <- genExpr e
      return [seqstm|return $expr:(e');|]
    Nothing -> return [seqstm|return;|]
genStm Break {} =
  -- TODO: rewrite if cond {break} to exit when cond
  return [seqstm|exit;|]
genStm Barrier {} =
  return [seqstm|wait until raising_edge(clk);|]

genGenerics :: TopDef -> [V.InterfaceDeclaration]
genGenerics p =
  mapMaybe
        -- FIMXE: We should probably generate VHDL native types here
        -- (i.e. integer)
    (\case
       (x, ConstPar t) ->
         let x' = toString x -- FIXME: This is silly
         in Just [ifacedecl|$ident:(x') : $subtyind:(genBuiltinType t)|]
       _ -> Nothing)
    ((params :: TopDef -> ParamList) p)
    --(M.toList ((params :: TopDef -> M.HashMap String ParamType) p))

-- toSignalName :: Ident -> Ident -> String
-- toSignalName i BusSignal {..} = toString i ++ "_" ++ toString name

genIdents :: [Ident] -> String
genIdents ns = intercalate "_" (map toString ns)

genPorts :: TopDef -> GenM ([V.InterfaceDeclaration], [V.SequentialStatement])
genPorts t = do
  (a, b) <- genPorts' t
  return (catMaybes a, catMaybes b)

-- Generate ports for a process using the naming_scheme:
-- If bus is declared within process, then name signal as bus-name_signal-name
-- If signal refers to a bus declared in another process then name as
-- bounding-proc_bus_name-signal_name
-- If signal refers to a bus imported via process paramaters, then name channels
-- as param-name_signal-name
genPorts' ::
     TopDef
  -> GenM ([Maybe V.InterfaceDeclaration], [Maybe V.SequentialStatement])
genPorts' ProcessTable {..} =
  unzip . concat <$>
  forM
    (concatMap (\(x, y) -> map (\a -> (x, a)) (S.toList y)) (M.toList usedBuses))
    (\(ref, (refAs, mode)) ->
       trace ("lookupDef ref " ++ show ref) $
       lookupDef ref >>= \case
         BusDef {busShape = BusShape bs} ->
           forM
             bs
             (\(sigName, (sigTy, sigVal)) ->
                let n = genIdents $ N.toList refAs <> [sigName]
                      -- case refAs of
                      --   Just a  -> genIdents [a, sigName]
                      --   Nothing -> genIdents [busName, sigName]
                in case mode of
                     Input ->
                       return
                         ( Just
                             [ifacedecl|signal $ident:n : in $subtyind:(genType sigTy)|]
                         , Nothing)
                     Output -> do
                       let dval =
                             case sigVal of
                               Just e  -> genLit e
                               Nothing -> [expr|0|]
                       return
                         ( Just
                             [ifacedecl|signal $ident:n :
                                       out $subtyind:(genType sigTy) :=
                                       $expr:dval|]
                         , Just [seqstm|$ident:n <= $expr:dval;|])
                     _ -> return (Nothing, Nothing))
         _ ->
           throw $
           InternalCompilerError
             "genPorts received a non-bus type from lookupDef")

genPorts' NetworkTable {} =
  return ([], [])

-- Add this as an entity declaration
genEntDec :: TopDef -> [V.InterfaceDeclaration] -> GenM V.LibraryUnit
genEntDec d ports = do
  let generics = genGenerics d
  let n = toString (nameOf d)
  if null generics then
    return [libraryunit|entity $ident:n is
                       port ( $ifacedecls:ports;
                       clk: in std_logic;
                       rst: in std_logic
                       );
                       end $ident:n;|]
  else
       return [libraryunit|entity $ident:n is
                       generic ( $ifacedecls:generics);
                       port ( $ifacedecls:ports;
                       clk: in std_logic;
                       rst: in std_logic);
                       end $ident:n;|]


-- genEntPorts :: TopDef -> GenM
-- genEntPorts = undefinedfvc

genVarDecls :: [DefType] -> GenM [V.ProcessDeclarativeItem]
genVarDecls dts =
  catMaybes <$>
  forM
    dts
    (\case
       VarDef {..} ->
         return $
         Just
           [procdecl|variable $ident:(toString varName) : $subtyind:(genType (typeOf varDef)) := $expr:(genLit varVal);|]
       ConstDef {..} ->
         return $
         Just
           [procdecl|constant $ident:(toString constName) : $subtyind:(genType (typeOf constDef)) := $expr:(genLit constVal);|]
       _ -> return Nothing)

genTopDef :: TopDef -> GenM [OutputFile]
genTopDef p@ProcessTable {..} = do
  let n = toString (nameOf p)
  (ports, resets) <- genPorts p
  ss <- mapM genStm stms
  ent <- genEntDec p ports
  decls <- genVarDecls (M.elems symTable)
  let contents =  [designfile|$contextitems:ctx
                             library work;
                             use work.sme_types.all;
                             $libraryunit:ent
                             architecture rtl of $ident:n is
                             begin
                               process (clk, rst)
                               $procdecls:decls
                                 begin
                                   if rst = '1' then
                                     $seqstms:resets
                                   elsif raising_edge(clk) then
                                     $seqstms:ss
                                   end if;
                                end process;
                             end rtl;|]
  return [OutputFile {destFile = n, ext = ".vhdl", content = V.pprrText contents}]
genTopDef nt@NetworkTable {..} = do
  -- We use the following naming scheme for signals used for interconnects:
  -- Signals of busses declared with a named instance are named
  -- process-name_instance-name_bus-name_signal-name
  -- Exception: Unique buses are named process-name_instance-name_bus-name
  --
  -- Buses declared within the network are named
  -- network-name_bus_name_signal_name
  --
  -- Buses declared within anonymous processes are named
  -- Process-name_Bus-name_Signal-name
  let n = toString (nameOf nt)
  return [OutputFile {destFile = n, ext = ".vhdl", content = T.empty}]
  -- let n = toString (nameOf td)
  -- return OutputFile {destFile = n, ext = ".vhdl", content = V.pprrText csvUtil}

genVHDL :: GenM OutputPlan
genVHDL =
  ([ OutputFile
     {destFile = "csvutil", ext = ".vhdl", content = V.pprrText csvUtil}
   ] ++) .
  concat <$>
  mapUsedTopDefsM (\x -> withScope (nameOf x) (genTopDef x))
