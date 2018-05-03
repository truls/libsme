{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module SME.CodeGen.VHDL (genVHDL) where

import           Control.Arrow               (first, (***))
import           Control.Exception           (throw)
import           Control.Monad               (forM, mapM, zipWithM)
import           Control.Monad.State         (gets)
import           Data.List                   (intercalate, nub, partition,
                                              sortOn)
import qualified Data.List.NonEmpty          as N
import           Data.Maybe                  (catMaybes, mapMaybe)
import qualified Data.Set                    as S
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import           Data.Text.Lazy              (toStrict)

import           Control.Monad.Extra         (concatForM, concatMapM)
import           Data.Generics.Uniplate.Data (universeBi)
import qualified Data.HashMap.Strict         as M
import           Data.Loc                    (noLoc)
import           Data.Makefile               (AssignmentType (SimpleAssign),
                                              Command (..),
                                              Entry (Assignment, OtherLine, Rule),
                                              Makefile (..), Target (Target))
import           Data.Makefile.Render        (encodeMakefile)
import           Data.String.Interpolate     (i)
import qualified Language.VHDL               as V
import           Language.VHDL.Quote         as Q
import           System.FilePath             ((<.>), (</>))

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax
import           SME.CodeGen.Common
import           SME.CodeGen.TH
import           SME.Error
import           SME.Representation

--import           Debug.Trace                 (trace, traceM)
--import           Text.Show.Pretty            (ppShow)

-- | Read the CSV parsing library
csvUtil :: V.DesignFile
csvUtil = $(parseVHDLToQexp "rts/vhdl/csv_util.vhdl")

typesFile :: String
typesFile = "sme_types"

-- | Return context items used in all files
ctx :: [V.ContextItem]
ctx = [contextitems|library ieee;
                   use ieee.std_logic_1164.all;
                   use ieee.numeric_std.all;|]

-- | VHDL representation of an SMEIL type
genType :: Typeness -> V.SubtypeIndication
genType (Typed (Unsigned Nothing _)) = [subtyind|integer|]
genType (Typed (Unsigned (Just 1) _)) = [subtyind|unsigned(1 downto 0)|]
genType (Typed (Unsigned (Just s) _)) =
  [subtyind|unsigned($lit:(s-1) downto 0)|]
genType (Typed (Signed Nothing _)) = [subtyind|integer|]
genType (Typed (Signed (Just s) _)) = [subtyind|signed($lit:(s-1) downto 0)|]
genType (Typed Array {..}) =
  let it = pprr (Typed innerTy)
      len = pprr arrLength
      end = "\\" :: String
      ty = [i|\\[#{len}]#{it}#{end}|]
  in [subtyind|$ident:ty|]
genType t@(Typed _) = genBuiltinType t
genType Untyped = [subtyind|untyped|]

-- | Generate VHDL builtin types such as integer instead of std_numeric type
genBuiltinType :: Typeness -> V.SubtypeIndication
genBuiltinType (Typed Signed {})   = [subtyind|integer|]
genBuiltinType (Typed Unsigned {}) = [subtyind|integer|]
genBuiltinType (Typed Bool {})     = [subtyind|boolean|]
genBuiltinType _                   = [subtyind|untyped|]

genName :: Name -> GenM V.Name
genName Name {..}
  -- TODO: Find a nicer way of generating names. Maybe we have to build the VHDL
  -- AST directly
 = do
  n <- intercalate "_" <$> mapM genNamePart (N.toList parts)
  return [Q.name|$ident:n|]

genNamePart :: NamePart -> GenM String
genNamePart IdentName {ident = i'} = pure (toString i')
genNamePart ArrayAccess {namePart = np, index = index} = do
  idx <- genArrayIdx index
  p <- genNamePart np
  return $ [i|#{V.pprr p}(to_integer(#{V.pprr idx}))|]

genArrayIdx :: ArrayIndex -> GenM V.Expression
genArrayIdx (Index e) = genExpr e
genArrayIdx Wildcard  = pure [expr|0|] -- FIXME: Change array index rep

-- | Generate VHDL code for a literal
genLit :: Literal -> GenM V.Expression
genLit l@LitInt {..} =
  getType >>= \case
    Typed (Signed (Just s) _) -> pure [expr|to_signed($lit:intVal, $lit:s)|]
    Typed (Unsigned (Just s) _) -> pure [expr|to_unsigned($lit:intVal, $lit:s)|]
    Untyped -> pure [expr|$lit:intVal|]
    t ->
      throw $
      InternalCompilerError
        ("Int literal with non-int type " ++ show t ++ " " ++ show l)
genLit LitFloat {..} = pure [expr|$lit:floatVal|]
genLit LitString {..} = pure [expr|$lit:stringVal|]
genLit LitArray {..}  =
            let
              elAssocs = map (\x -> [elassoc|$lit:x|]) arrayVal
            in
              pure [expr|($elassocs:elAssocs)|]
genLit LitTrue {..} = pure [expr|true|]
genLit LitFalse {..} = pure [expr|false|]

-- | Generate cooresponding VHDL code for an expression
genExpr :: Expr -> GenM V.Expression
genExpr Binary {..} = do
  e1 <- genExpr left
  e2 <- genExpr right
  -- TODO: "Unparse" correctly. See:
  -- https://www.cs.tufts.edu/~nr/pubs/unparse-abstract.html
  -- I'm not sure if SMEIL and VHDL has the same operator precedences so for now
  -- we just place parentheses around all binops.
  return [expr|($expr:(genBinOp binOp e1 e2))|]
  where
    genBinOp (PlusOp _)  e1 e2 = [expr|$expr:e1  +  $expr:e2|]
    genBinOp (MinusOp _) e1 e2 = [expr|$expr:e1  -  $expr:e2|]
    genBinOp (MulOp _)   e1 e2 = [expr|$expr:e1  *  $expr:e2|]
    genBinOp (DivOp _)   e1 e2 = [expr|$expr:e1  /  $expr:e2|]
    genBinOp (ModOp _)   e1 e2 = [expr|$expr:e1 mod $expr:e2|]
    genBinOp (EqOp _)    e1 e2 = [expr|$expr:e1  =  $expr:e2|]
    genBinOp (NeqOp _)   e1 e2 = [expr|$expr:e1 /=  $expr:e2|]
    genBinOp (SllOp _)   e1 e2 = [expr|$expr:e1 sll $expr:e2|]
    genBinOp (SrlOp _)   e1 e2 = [expr|$expr:e1 srl $expr:e2|]
    genBinOp (LtOp _)    e1 e2 = [expr|$expr:e1  <  $expr:e2|]
    genBinOp (GtOp _)    e1 e2 = [expr|$expr:e1  >  $expr:e2|]
    genBinOp (LeqOp _)   e1 e2 = [expr|$expr:e1 <=  $expr:e2|]
    genBinOp (GeqOp _)   e1 e2 = [expr|$expr:e1 >=  $expr:e2|]
    genBinOp (AndOp _)   e1 e2 = [expr|$expr:e1 and $expr:e2|]
    genBinOp (OrOp _)    e1 e2 = [expr|$expr:e1 or  $expr:e2|]
    genBinOp (XorOp _)   e1 e2 = [expr|$expr:e1 xor $expr:e2|]
    genBinOp (ConOp _)   e1 e2 = [expr|$expr:e1 and $expr:e2|]
    genBinOp (DisOp _)   e1 e2 = [expr|$expr:e1 or  $expr:e2|]

genExpr Unary {..} = do
  e <- genExpr right
  return $ genUnOp unOp e
  where
    genUnOp (UnPlus _)  e = [expr|+$expr:e|]
    genUnOp (UnMinus _) e = [expr|-$expr:e|]
    genUnOp (NotOp _)   e = [expr|not $expr:e|]
    genUnOp (NegOp _)   e = [expr|not $expr:e|]

genExpr PrimName {name = n} = do
  n' <- genName n
  return [expr|$name:(n')|]
   -- let
   -- name = fold (\n m -> [Q.name|$name:n . $name:m|]
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
genExpr PrimLit {lit = l} =   genLit l
genExpr Parens {innerExpr = ex} = do
  e <- genExpr ex
  return [expr|($expr:e)|]

-- | Generate VHDL code for an SEMIL statement
genStm :: Statement -> GenM V.SequentialStatement
genStm Assign {..} = do
  e <- withType genExpr val
  d <- genName dest
  --let len = [Q.name|foo'length|]
  let target =
        case typeOf val of
          Typed (Signed _ _)   -> [expr|resize($expr:e , $name:d'length)|]
          Typed (Unsigned _ _) -> [expr|resize($expr:e , $name:d'length)|]
          _                    -> [expr|$expr:e|]
          -- return [seqstm|$ident:d <= $expr:e;|]
  lookupDef dest >>= \case
    BusDef {} -> return [seqstm|$name:d <= $expr:target;|]
    VarDef {} -> return [seqstm|$name:d := $expr:target;|]
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
  return [seqstm|case to_integer($expr:val) is $casealts:(c' ++ dc) end case;|]
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
  return [seqstm|wait until rising_edge(clk);|]
genStm Trace {} = return [seqstm|null;|]
genStm Assert {..} = do
  e <- genExpr cond
  l <-
    case descr of
      Just s  -> Just <$> genLit s
      Nothing -> pure Nothing
  case l of
    Just s -> return [seqstm|assert $expr:e report $expr:s severity failure;|]
    Nothing -> return [seqstm|assert $expr:e severity failure;|]


genGenerics :: TopDef -> [V.InterfaceDeclaration]
genGenerics p =
  mapMaybe
    (\case
       (x, ConstPar t) ->
         Just [ifacedecl|$ident:(toString x) : $subtyind:(genBuiltinType t)|]
       _ -> Nothing)
    ((params :: TopDef -> ParamList) p)
    --(M.toList ((params :: TopDef -> M.HashMap String ParamType) p))

-- toSignalName :: Ident -> Ident -> String
-- toSignalName i BusSignal {..} = toString i ++ "_" ++ toString name


genIdents :: [Ident] -> String
genIdents ns = intercalate "_" (map toString ns)

getUsedBuses :: TopDef -> [(Ref, (Ref, BusState))]
getUsedBuses ProcessTable {..} =
  concatMap (\(x, y) -> map (\a -> (x, a)) (S.toList y)) (M.toList usedBuses)
getUsedBuses NetworkTable {} =
  error "Instantiation of networks not yet supported"

-- Generate ports for a process using the naming_scheme:
-- If bus is declared within process, then name signal as bus-name_signal-name
-- If signal refers to a bus declared in another process then name as
-- bounding-proc_bus_name-signal_name
  -- or maybe \net_name.inst_name(proc_name)[i].sig_name\
-- If signal refers to a bus imported via process paramaters, then name channels
-- as param-name_signal-name
genPorts ::
     TopDef
  -> GenM ([V.InterfaceDeclaration], [V.SequentialStatement])
genPorts pt@ProcessTable {..} =
  (catMaybes *** catMaybes) . unzip . concat <$>
  forM
    (getUsedBuses pt)
    (\(ref, (refAs, mode)) ->
       lookupDef ref >>= \case
         BusDef {busShape = BusShape bs} ->
           forM
             bs
             (\(sigName, (sigTy, sigVal, _)) ->
                let n = genIdents $ N.toList refAs <> [sigName]
                      -- case refAs of
                      --   Just a  -> genIdents [a, sigName]
                      --   Nothing -> genIdents [busName, sigName]
                in case mode of
                     Input ->
                       return
                         ( Just
                             [ifacedecl|signal $ident:n : in
                                       $subtyind:(genType sigTy)|]
                         , Nothing)
                     Output -> do
                       dval <- genDefaultExpr sigVal sigTy
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
genPorts NetworkTable {} =
  return ([], [])

-- | Return an expression setting the default value for a type
genDefaultExpr :: Maybe Literal -> Typeness -> GenM V.Expression
genDefaultExpr sigVal ty =
  case sigVal of
    Just e -> withType genLit e
    Nothing ->
      case ty of
        Typed Array {..} -> do
          dl <- genDefaultLit (Typed innerTy)
          l <- withType' (Typed innerTy) (genLit dl)
          return [expr|(others => $expr:l)|]
        _ -> do
          dl <- genDefaultLit ty
          withType' ty (genLit dl)

genDefaultLit :: Typeness -> GenM Literal
genDefaultLit Untyped = throw $ InternalCompilerError "Untyped value in gen"
genDefaultLit (Typed t) = go t
  where
    go Signed {}   = return $ LitInt 0 noLoc
    go Unsigned {} = return $ LitInt 0 noLoc
    go Bool {}     = return $ LitFalse noLoc
    -- FIXME: This should depend on the length of the array type
    go Array {}    = return $ LitArray [0] noLoc
    go Single {}   = return $ LitFloat 0 noLoc
    go Double {}   = return $ LitFloat 0 noLoc
    go String {}   = return $ LitString "" noLoc

genImageFun :: String -> Typeness -> V.Expression
genImageFun n (Typed (Signed _ _))   = [expr|int_image($ident:n)|]
genImageFun n (Typed (Unsigned _ _)) = [expr|uint_image($ident:n)|]
genImageFun n (Typed (Bool _))       = [expr|bool_image($ident:n)|]
genImageFun n _                      = [expr|$ident:n|]

genValReader :: String -> Typeness -> V.SequenceOfStatements
genValReader n (Typed (Signed _ _))   = genIntValReader n
genValReader n (Typed (Unsigned _ _)) = genIntValReader n
genValReader n (Typed (Bool _)) = [seqstms|read_csv_field(L, tmp);
if are_strings_equal(tmp, "U") then
   $ident:n := false;
else
   $ident:n := to_value(truncate(tmp));
end if;
fieldno := fieldno + 1;|]
genValReader _ _ = error "Value not yet supported"

genIntValReader :: String -> V.SequenceOfStatements
genIntValReader x = [seqstms|read_csv_field(L, tmp);
if are_strings_equal(tmp, "U") then
   $ident:x := (others => 'U');
else
   $ident:x := to_value(truncate(tmp), $ident:x'length);
end if;
fieldno := fieldno + 1;|]

genReadValDecls :: String -> Typeness -> V.ProcessDeclarativeItem
genReadValDecls n ty = [procdecl|variable $ident:n: $subtyind:(genType ty);|]

genValSetter :: String -> String -> V.SequentialStatement
genValSetter x y = [seqstm|$ident:x <= $ident:y;|]

genTB :: String -> ([(String, Typeness)], [(String, Typeness)]) -> [TaggedFile]
genTB entName (ins, outs) =
  let tbName = [i|#{entName}_tb|]
      inTmpVals = map (first (\x -> [i|#{x}_val|])) ins
      outTmpVals = map (first (\x -> [i|#{x}_val|])) outs
      tmpVals = sortOn fst $ inTmpVals ++ outTmpVals

      ports = --trace ("Ins " ++ show ins ++ " Outs " ++ show outs)
              sortOn fst $ ins ++ outs
      readVarDecls = map (uncurry genReadValDecls) tmpVals
      signals = map (\(n, t) -> [blockdecl|signal $ident:n:
                                          $subtyind:(genType t);|]) ports
      maps = map (\(n, _) -> [assocel|$ident:n => $ident:n|]) ports
      fieldAssert (x, _) = [seqstms|read_csv_field(L, tmp);
                                   assert are_strings_equal(tmp, $slit:x)
                                   report "Field #" & integer'image(fieldno) &
                                   " is named: " & truncate(tmp) &
                                   $slit:(" but expected " ++ x)
                                   severity Failure;
                                   fieldno := fieldno + 1;|]
      fieldAsserts = concatMap fieldAssert ports
      valReaders = concatMap (uncurry genValReader) tmpVals
      valAssert (y, _) (x, ty)= let im = genImageFun y ty
                                    im2 = genImageFun x ty in
        [seqstm|assert ($ident:y = $ident:x)
    report $slit:("Unexpected value of " ++ x ++ " in cycle ") &
    integer'image(clockcycle) & ". Actual value was: " &
    $expr:im2 & " but expected " & $expr:im
    severity Error;
end if;|]
      valAsserts = zipWith valAssert outTmpVals outs
      valWrites = zipWith genValSetter (map fst ins) (map fst inTmpVals)
      contents = [designfile|$contextitems:ctx
use std.textio.all;
library work;
use work.csv_util.all;
entity $ident:tbName is end $ident:tbName;
architecture TB of $ident:tbName is
  $blockdecls:signals
  signal clock: std_logic;
  signal stop_clock: boolean;
  signal reset: std_logic;
begin
  uut: entity work.$ident:entName
  port map (
    $assocels:maps,
    rst => reset,
    clk => clock);
    clk: process
    begin
      while not stop_clock loop
        clock <= '1';
        wait for 5 ms;
        clock <= '0';
        wait for 5 ns;
      end loop;
      wait;
    end process;

TraceFileTester: process
    file F: TEXT;
    variable L: LINE;
    variable Status: FILE_OPEN_STATUS;
    constant filename : string := "../trace.csv";
    variable clockcycle : integer := 0;
    variable tmp : CSV_LINE_T;
    variable readOK : boolean;
    variable fieldno : integer := 0;
    variable failures : integer := 0;
    variable newfailures: integer := 0;
    variable first_failure_tick : integer := -1;
    $procdecls:readVarDecls

begin

    FILE_OPEN(Status, F, filename, READ_MODE);
    if Status /= OPEN_OK then
        report "Failed to open CSV trace file" severity Failure;
    else
    readline(f, l);
    fieldno := 0;

      $seqstms:fieldAsserts

      reset <= '1';
      wait for 5 ns;
      reset <= '0';

      -- Read a line each clock
      while not ENDFILE(F) loop
      READLINE(F, L);

       fieldno := 0;
       newfailures := 0;

       -- Read all signals to variables
       $seqstms:valReaders

       -- Write all driver signals out
       $seqstms:valWrites

       wait until rising_edge(clock);

       $seqstms:valAsserts

       failures := failures + newfailures;
       if newfailures = 0 then
          first_failure_tick := -1;
       elsif first_failure_tick = -1 then
          first_failure_tick := clockcycle;
       else
          if clockcycle - first_failure_tick >= 5 then
            report "Stopping simulation due to five consecutive failed cycles"
            severity error;
            stop_clock <= true;
          elsif failures > 20 then
            report "Stopping simulation after 20 failures" severity error;
            stop_clock <= true;
          end if;
        end if;

       clockcycle := clockcycle + 1;
       end loop;

      FILE_CLOSE(F);
      end if;

      if failures = 0 then
        report "completed successfully after " & integer'image(clockcycle) & " clockcycles";
      else
        report "completed with " & integer'image(failures) & " error(s) after " & integer'image(clockcycle) & " clockcycle(s)";
      end if;
      stop_clock <= true;

    wait;
end process;
end architecture TB;
  |]
        in
  [ TestBench OutputFile { destFile = tbName
                         , fileExt = "vhdl"
                         , content = V.pprrText contents, deps = [ "csv_util"
                                                                 , typesFile
                                                                 , entName ] } ]

-- Add this as an entity declaration
genEntDec :: TopDef -> [V.InterfaceDeclaration] -> GenM V.LibraryUnit
genEntDec d ports = do
  let generics = genGenerics d
  let n = toString (nameOf d)
  if null generics
    then return
           [libraryunit|entity $ident:n is
                       port ( $ifacedecls:ports;
                       clk: in std_logic;
                       rst: in std_logic
                       );
                       end $ident:n;|]
    else return
           [libraryunit|entity $ident:n is
                       generic ( $ifacedecls:generics);
                       port ( $ifacedecls:ports;
                       clk: in std_logic;
                       rst: in std_logic);
                       end $ident:n;|]

-- | Generate variable declarations and reset statements
genVarDecls ::
     [DefType] -> GenM ([V.ProcessDeclarativeItem], [V.SequentialStatement])
genVarDecls dts =
  unzip . catMaybes <$>
  forM
    dts
    (\case
       VarDef {..} -> do
         dv <- genDefaultExpr varVal (typeOf varDef)
         return $
           Just
             ( [procdecl|variable $ident:(toString varName) :
                         $subtyind:(genType (typeOf varDef)) := $expr:dv;|]
             , [seqstm|$ident:(toString varName) := $expr:dv;|])
       _ -> return Nothing)


-- FIXME: Eliminate the code duplication of the two following functions
-- | Geneartes constant declarations
genConstDecls :: [DefType] -> GenM [V.ProcessDeclarativeItem]
genConstDecls dts =
  catMaybes <$>
  forM
    dts
    (\case
       ConstDef {..} -> do
         litVal <- withType' Untyped (genLit constVal)
         return $
           Just
             [procdecl|constant $ident:(toString constName) :
                      $subtyind:(genBuiltinType (typeOf constDef)) :=
                      $expr:litVal;|]
       _ -> return Nothing)

-- | Geneartes constant declarations
genConstDeclsBlock :: [DefType] -> GenM [V.BlockDeclarativeItem]
genConstDeclsBlock dts =
  catMaybes <$>
  forM
    dts
    (\case
       ConstDef {..} -> do
         litVal <- withType' Untyped (genLit constVal)
         return $
           Just
             [blockdecl|constant $ident:(toString constName) :
                      $subtyind:(genBuiltinType (typeOf constDef)) :=
                      $expr:litVal;|]
       _ -> return Nothing)


-- | Genearte VHDL code for instance declarations
genInstDecls :: [DefType] -> GenM [(V.ConcurrentStatement, Ident)]
genInstDecls dts =
  catMaybes <$>
  mapM
    (\case
       InstDef { instantiated = instantiated
               , instDef = instDef@Instance {}
               , params = params
               , anonymous = anon
               } -> do
         inst <- lookupTopDef instantiated
         -- Get generic association maps
         constAssocs <- genGenericMap (refOf instantiated) instDef
         -- Get port association maps
         -- Generate the buses that we can from the parameter lists
         fromParams <- genPortParamMap instantiated params
         let fromParamRefs = map fst fromParams
             remainingBuses =
               filter (\x -> fst x `elem` fromParamRefs) (getUsedBuses inst)
             fromParams' = concatMap snd fromParams
         portAssocs <-
           concatForM
             remainingBuses
             (\(ref, (refAs, _)) -> do
                bus <- lookupDef ref
                let n = genChName anon (nameOf instDef) (nameOf bus)
                return $ genPortMap n refAs (busShape (bus :: DefType)))
         return $
           let n = genInstName anon (nameOf inst) (nameOf instDef)
           in if null constAssocs
                then Just
                       ( [constm|$ident:n:
                           entity work.$ident:(toString $ procName inst)
                           port map (
                             $assocels:(portAssocs ++ fromParams'),
                             clk => clk,
                             rst => rst);
                           |]
                       , procName inst)
                else Just
                       ( [constm|$ident:n:
                           entity work.$ident:(toString $ procName inst)
                           generic map ($assocels:constAssocs)
                           port map (
                             $assocels:(portAssocs ++ fromParams'),
                             clk => clk,
                             rst => rst);
                           |]
                       , procName inst)
             -- Only generate input/output buses if this is an exposed bus
       _ -> pure Nothing)
    dts
  where
    genInstName :: Bool -> Ident -> Ident -> String
    genInstName anon pName iName =
      toString $
      if anon
        then pName
        else iName
    genPortParamMap ::
         Ident -> [InstParam] -> GenM [(Ref, [V.AssociationElement])]
    genPortParamMap topDef pars = do
      forParList <- (params :: TopDef -> ParamList) <$> lookupTopDef topDef
      catMaybes <$>
        zipWithM
          (\(_, parType) p ->
             case (parType, p) of
               (BusPar {..}, InstBusPar par) -> do
                 busName <- refOf . nameOf <$> lookupDef ref
                 return $ Just (ref, genPortMap busName par parBusShape)
               _ -> return Nothing)
          forParList
          pars

-- | Generate the generic maps for an instance
genGenericMap :: Ref -> Instance -> GenM [V.AssociationElement]
genGenericMap topDef Instance {params = instPars} = do
  -- TODO: Maybe rewrite to use the params list in InstDef
  inst <- lookupTopDef topDef
  let forParList = (params :: TopDef -> ParamList) inst
      parList = zipWith (\(a, b) (_, c) -> (a, b, c)) forParList instPars
  catMaybes <$>
    forM
      parList
      (\case
         (n, ConstPar {}, e) -> do
           e' <- withType' Untyped $ genExpr e
           pure $ Just [assocel|$ident:(toString n) => $expr:(e')|]
         (_, BusPar {}, _) -> pure Nothing)

-- For buses that are not exposed, we simply use signals for the
-- interconnect. For exposed buses, create an appropriate in/out port in the
-- entity and connect to that.

-- | Gathers all buses used by instantiated processes. If they are exposed, we
-- generate the corresponding signals and otherwise we genearte the appropriate
-- input/output ports.
getUsedBusesOfInsts :: [DefType] -> GenM [(Maybe BusState, Ref, BusShape)]
getUsedBusesOfInsts dts = do
  buses <-
    concatForM
      dts
      (\case
         InstDef { instantiated = instantiated
                 , anonymous = anonymous
                 , instName = instName
                 } -> do
           inst <- lookupTopDef instantiated
           return $ map (, instName, anonymous) (getUsedBuses inst)
         _ -> pure [])
  concatForM
    buses
    -- FIXME: Woha!
    (\((def, (_, state)), instName, anonymous) ->
       lookupDef def >>= \case
         bd@BusDef {isExposed = ex, busShape = bs} ->
           let n = genChName anonymous instName (nameOf bd)
           in case (ex, state) of
                (True, _)       -> pure [(Just state, n, bs)]
                -- For non-exposed buses, we only add connective signals for
                -- outwards-facing connections. That way, we prevent creating
                -- duplicated signal names.
                (False, Output) -> pure [(Nothing, n, bs)]
                (False, _)      -> pure []
         _ -> throw $ InternalCompilerError "Bus is not a bus")


genChName :: Bool -> Ident -> Ident -> Ref
genChName True _iName bName = refOf bName
genChName False iName bName = refOf iName <> refOf bName

genInstSignals :: [DefType] -> GenM V.ArchitectureDeclarativePart
genInstSignals dt = genInstSignals' =<< getUsedBusesOfInsts dt
  where
    genInstSignals' ::
         [(Maybe BusState, Ref, BusShape)] -> GenM V.ArchitectureDeclarativePart
    genInstSignals' a = catMaybes <$> concatMapM go a
      where
        go (Nothing, ref, shape) =
          forM
            (unBusShape shape)
            (\(sigName, (ty, l, _)) -> do
               let n = genIdents $ N.toList ref <> [sigName]
               dval <- genDefaultExpr l ty
               return $
                 Just
                   [blockdecl|signal $ident:n :
                          $subtyind:(genType ty) := $expr:dval;|])
        go _ = return []

-- TODO: Now that we have ranges in busShapes, we can also render ranges in the
-- generated VHDL

-- | Generates external entity signals from exposed buses
genExtPorts :: [DefType] -> GenM [V.InterfaceDeclaration]
genExtPorts dt = genExtPorts' =<< getUsedBusesOfInsts dt
  where
    genExtPorts' ::
         [(Maybe BusState, Ref, BusShape)] -> GenM [V.InterfaceDeclaration]
    genExtPorts' a = catMaybes <$> concatMapM go a
      where
        go (Just mode, ref, shape) =
          forM
            (unBusShape shape)
            (\(sigName, (ty, l, _))  ->
               let n = genIdents $ N.toList ref <> [sigName]
               in case mode of
                    Input ->
                      return $
                      Just
                        [ifacedecl|signal $ident:n : in $subtyind:(genType ty)|]
                    Output -> do
                      dval <- genDefaultExpr l ty
                      return $
                        Just
                          [ifacedecl|signal $ident:n :
                                out $subtyind:(genType ty) := $expr:dval;|]
                    _ -> pure Nothing)
        go _ = return []

-- | Generates the names and types of input and output buses for use with the
-- test bench generation
genTBWires :: [DefType] -> GenM ([(String, Typeness)], [(String, Typeness)])
genTBWires dt = genTBWires' <$> getUsedBusesOfInsts dt

genTBWires' ::
     [(Maybe BusState, Ref, BusShape)]
  -> ([(String, Typeness)], [(String, Typeness)])
genTBWires' l =
  let (ins, outs) =
        partition
          (\(x, _, _) -> x == Input)
          (sortOn (\(_, x, _) -> x) $ concatMap go l)
  in (map dropFirst ins, map dropFirst outs)
  where
    go (Just mode, ref, shape) =
      map
        (\(sigName, (ty, _, _)) ->
           let n = genIdents $ N.toList ref <> [sigName]
           in (mode, n, ty))
        (unBusShape shape)
    go (Nothing, _, _) = []
    dropFirst (_, x, y) = (x, y)


-- | Generates port mappings
-- Generate mappings remote name => local name
--genPortMap :: ParamType
genPortMap :: Ref -> Ref -> BusShape -> [V.AssociationElement]
genPortMap busName localRef = map go . unBusShape
  where
    go (sigName, (_, _, _)) =
      let theirName = genIdents $ N.toList localRef <> [sigName]
          myName = genIdents $ N.toList (busName <> refOf sigName)
      in [assocel|$ident:theirName => $ident:myName|]

-- | Generates a VHDL file containing an SMEIL process
genTopDef :: TopDef -> GenM [TaggedFile]
genTopDef p@ProcessTable {..} = do
  let n = toString (nameOf p)
  (ports, sigResets) <- genPorts p
  ss <- mapM genStm stms
  ent <- genEntDec p ports
  (decls, varResets) <- genVarDecls (M.elems symTable)
  consts <- genConstDecls (M.elems symTable)
  let contents =
        [designfile|$contextitems:ctx
                   library work;
                   use work.sme_types.all;
                   $libraryunit:ent
                   architecture rtl of $ident:n is
                   begin
                     process (clk, rst)
                       $procdecls:consts
                       $procdecls:decls
                     begin
                       if rst = '1' then
                         $seqstms:sigResets
                         $seqstms:varResets
                       elsif rising_edge(clk) then
                         $seqstms:ss
                       end if;
                     end process;
                   end rtl;|]
  return
    [ Regular
        OutputFile
        { destFile = n
        , fileExt = ".vhdl"
        , content = V.pprrText contents
        , deps = [typesFile]
        }
    ]


genTopDef nt@NetworkTable {..}
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
 = do
  let n = toString (nameOf nt)
  let decls = M.elems symTable
  (instDecls, deps) <- unzip <$> genInstDecls decls
  intSigs <- genInstSignals decls
  consts <- genConstDeclsBlock (M.elems symTable)
  extPorts <- genExtPorts decls
  let contents =
        [designfile|$contextitems:ctx
                            library work;
                            entity $ident:n is
                              port (
                                $ifacedecls:extPorts;
                                clk: in std_logic;
                                rst: in std_logic);
                            end $ident:n;
                            architecture rtl of $ident:n is
                              $blockdecls:intSigs
                              $blockdecls:consts
                            begin
                              $constms:instDecls
                            end rtl;
                            |]
  tb <-
    if topLevel
      then do
        wires <- genTBWires decls
        return $ genTB n wires
      else pure []
  return $
    Regular
      OutputFile
      { destFile = n
      , fileExt = ".vhdl"
      , content = V.pprrText contents
      , deps = typesFile : map toString deps
      } :
    tb


-- | Generates a file containing array definitions
genTypeDefs :: GenM TaggedFile
genTypeDefs = do
  ds <- M.elems <$> gets defs
  let types = nub $ universeBi ds :: [Type]
  res <- catMaybes <$> mapM (\case
               Array {..} -> do
                 let it = pprr (Typed innerTy)
                     len = pprr arrLength
                     ai = [i|\\[#{len}]#{it}\\|]
                 ex <- case arrLength of
                   Just l  -> genExpr l
                   -- TODO: Change the internal type representation such that
                   -- this isn't needed
                   Nothing -> pure [expr|0|]
                 return $ Just $ [packdeclit|type $ident:ai is array
                                            (0 to $expr:ex) of
                                            $subtyind:(genType (Typed innerTy));|]
               _ -> pure Nothing
           ) types
  let contents = [designfile|library ieee;
use ieee.numeric_std.all;

package sme_types is
$packdeclits:res
end package;|]
  return $ Regular OutputFile {destFile = "sme_types", fileExt = ".vhdl", content = V.pprrText contents, deps = []}

genMakefile :: [TaggedFile] -> OutputPlan
genMakefile tfs
     --(os, ms) = trace (ppShow tfs) unzip $ map go tfs
 =
  let (os, ms) = unzip $ map go tfs
      ms' = mconcat ms
      tbName = getTBName tfs
      allTarget = Rule "all" [fromString tbName] []
      header =
        [ OtherLine "# Makefile genearted by libSME"
        , OtherLine ""
        , Assignment SimpleAssign "WORKDIR" "work"
        , Assignment SimpleAssign "STD" "08"
        , Assignment SimpleAssign "VCDFILE" "trace.vcd"
        , OtherLine ""
        , OtherLine ""
        , allTarget
        , OtherLine ""
        , Rule (Target "$(WORKDIR)") [] [Command "mkdir $(WORKDIR)"]
        ]
  in OutputFile
     { destFile = "Makefile"
     , fileExt = ""
     , content = toStrict $ encodeMakefile Makefile {entries = header ++ ms'}
     , deps = []
     } :
     os
  where
    go :: TaggedFile -> (OutputFile, [Entry])
    go (Regular f@OutputFile {..}) =
      ( f
      , [ Rule
            (Target (fromString $ "$(WORKDIR)" </> destFile <.> "o"))
            (map fromString (fromString (fileName f) : genDeps deps))
            [Command $ analyze f]
        ])
    go (TestBench f@OutputFile {..}) =
      ( f
      , let tbObj = ("$(WORKDIR)" </> destFile <.> "o")
        in [ Rule
               (Target $ fromString tbObj)
               (map fromString (fromString (fileName f) : genDeps deps))
               [Command $ analyze f]
           , Rule
               (Target $ fromString destFile)
               ["$(WORKDIR)", fromString tbObj]
               [Command $ elaborate destFile]
           ])
    genDeps = map (\x -> "$(WORKDIR)" </> x <.> "o")
    getTBName []                            = ""
    getTBName (TestBench OutputFile {..}:_) = destFile
    getTBName (_:rest)                      = getTBName rest
    analyze f =
      T.pack [i|ghdl -a --std=$(STD) --workdir=$(WORKDIR) #{fileName f}|]
    elaborate e = T.pack [i|ghdl -e --std=$(STD) --workdir=$(WORKDIR) #{e}|]

data TaggedFile
  = Regular OutputFile
  | TestBench OutputFile

genVHDL :: GenM OutputPlan
genVHDL = do
  td <- genTypeDefs
  genMakefile .
    ([ Regular
         OutputFile
         { destFile = "csv_util"
         , fileExt = "vhdl"
         , content = V.pprrText csvUtil
         , deps = []
         }
     , td
     ] ++) .
    concat <$>
    mapUsedTopDefsM (\x -> withScope (nameOf x) (genTopDef x))
