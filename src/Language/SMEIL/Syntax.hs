-- | This module defines the syntax for the SME intermediate
-- representation. For details, see: TODO/langspec.pdf

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.SMEIL.Syntax
  ( DesignFile(..)
  , DesignUnit(..)
  , UnitElement(..)
  , Import(..)
  , Instance(..)
  , Param(..)
  , Network(..)
  , NetworkDecl(..)
  , Bus(..)
  , BusSignal(..)
  , Range(..)
  , Process(..)
  , Generate(..)
  , Declaration(..)
  , Variable(..)
  , Constant(..)
  , Function(..)
  , Statement(..)
  , Enumeration(..)
  , Direction(..)
  , Expr(..)
  , BinOp(..)
  , UnOp(..)
  , Name(..)
  , NamePart(..)
  , ArrayIndex(..)
  , Type(..)
  , Literal(..)
  , Ident(..)
  , ToString(..)
  , Nameable(..)
  , Typeness(..)
  , Typed(..)
  , References(..)
  , Ref
  , bitSize
  ) where

import           Data.Data                       (Data, Typeable)
import           Data.Hashable                   (Hashable (hashWithSalt))
import qualified Data.List.NonEmpty              as N
import           Data.Loc                        (Located (locOf), SrcLoc,
                                                  noLoc)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import           Text.PrettyPrint.Mainland       (cat, dot, punctuate)
import           Text.PrettyPrint.Mainland.Class (Pretty (ppr))

--import           Debug.Trace                     (trace)
trace :: String -> b -> b
trace _ = id

class ToString a where
  toString :: a -> String
  toText :: a -> T.Text

class Nameable a where
  nameOf :: a -> Ident

type Ref = N.NonEmpty Ident

-- instance Located Ref where
--   locOf (

instance Pretty Ref where
  ppr r = cat $ punctuate dot (map (\(Ident i _) -> ppr i) (N.toList r))

class References a where
  refOf :: a -> Ref

instance References Ref where
  refOf r = r

data Typeness
  = Typed Type
  | Untyped
  deriving (Ord, Eq, Show, Data, Typeable)

instance Located Typeness where
  locOf (Typed t) = locOf t
  locOf Untyped   = noLoc

class Typed a where
  typeOf :: a -> Typeness

data DesignFile = DesignFile
  { units :: [DesignUnit]
  , loc   :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located DesignFile where
  locOf DesignFile {..} = locOf loc

instance Semigroup DesignFile where
  (DesignFile un loc) <> (DesignFile un' _) = DesignFile (un ++ un') loc

data DesignUnit = DesignUnit
  { imports     :: [Import] -- ^ Imports of the design unit
  , unitElement :: [UnitElement] -- ^ loc unit-level process or network
  , loc         :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located DesignUnit where
  locOf DesignUnit {..} = locOf loc

data UnitElement
  = UnitProc { process :: Process }
  | UnitNet { network :: Network }
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Specifies loc module to be imported in current design module
data Import
  = SimpleImport { modName   :: Ref -- ^ Name of the module to be imported
                ,  qualified :: Maybe Ident -- ^ Optional qualified name of import
                ,  loc       :: SrcLoc}
  | SpecificImport { modName   :: Ref -- ^ Name of module to be imported
                  ,  entities  :: [Ident] -- ^ Entities from module to be imported
                  ,  qualified :: Maybe Ident -- ^ Optional qualified name of import
                  ,  loc       :: SrcLoc}
  deriving (Eq, Ord, Show, Data, Typeable)

instance Located Import where
  locOf SimpleImport {..}   = locOf loc
  locOf SpecificImport {..} = locOf loc

-- | Instantiates either loc "Process" or loc "Network"
data Instance = Instance
  {
    instName  :: Maybe Ident -- ^ The name of the instance
    -- TODO: Change to this:
    --instName  :: (Maybe Ident, SrcLoc) -- ^ The name of the instance
  , instIndex :: Maybe Expr
  , elName    :: Name -- ^ The name of the object to initialize
  , params    :: [(Maybe Ident, Expr)] -- ^ Optionally named parameters of the object
  , loc       :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Instance where
  locOf Instance {..} = locOf loc

instance Nameable Instance where
  nameOf Instance {..} =
    fromMaybe (Ident "" noLoc) instName -- TODO: What to do here

-- | Describes loc parameter used in the specification of "Process" or "Network"
data Param = Param
  { count :: Maybe (Maybe Expr) -- ^ Length of locrray of params
  , dir   :: Direction -- ^ Parameter direction
  , name  :: Ident -- ^ Parameter name
  , loc   :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Param where
  locOf Param {..} = locOf loc

-- | Defines loc Network
data Network = Network
  { name     :: Ident -- ^ Name of network
  , params   :: [Param] -- ^ Network parameters
  , netDecls :: [NetworkDecl] -- ^ Declarations in network
  , loc      :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Network where
  locOf Network {..} = locOf loc

instance Nameable Network where
  nameOf Network {..} = name

data NetworkDecl
  = NetInst { inst :: Instance
              -- ^ loc network instance
             }
  | NetBus { bus :: Bus
           -- ^ loc network declaration
            }
  | NetConst { const :: Constant
             -- ^ loc network constant
              }
  | NetGen { gen :: Generate
           -- ^ Generator statement
            }
  deriving (Eq, Ord, Show, Data, Typeable)

data Bus = Bus
  { exposed :: Bool
  -- ^Bus is exposed on top level
  , unique  :: Bool
  -- ^Bus is unique, i.e., not duplicated on process instantiation
  , name    :: Ident
  -- ^Name of bus
  , signals :: [BusSignal]
  -- ^Bus signals
  , loc     :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Bus where
  locOf Bus {..} = locOf loc

instance Nameable Bus where
  nameOf Bus {..} = name

data BusSignal = BusSignal
  { name  :: Ident -- ^Name of signal
  , ty    :: Typeness -- ^Type of signal
  , value :: Maybe Expr -- ^Initial value of signal
  , range :: Maybe Range -- ^Signal range
  , loc   :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Typed BusSignal where
  typeOf BusSignal {..} = ty

instance Located BusSignal where
  locOf BusSignal {..} = locOf loc

data Range = Range
  { lower :: Expr -- ^Lower bound
  , upper :: Expr -- ^Upper bound
  , loc   :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Range where
  locOf Range {..} = locOf loc

data Process = Process
  { name   :: Ident -- ^Name of process
  , params :: [Param] -- ^Process parameters
  , decls  :: [Declaration] -- ^Process declarations
  , body   :: [Statement] -- ^Process body
  , sync   :: Bool -- ^ Process is synchronous
  , loc    :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Nameable Process where
  nameOf Process {..} = name

instance Located Process where
  locOf Process {..} = locOf loc

-- | Generator expression for use in Processes
data Generate = Generate
  { var     :: Ident
  , from    :: Expr
  , to      :: Expr
  , genBody :: [NetworkDecl]
  , loc     :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Located Generate where
  locOf Generate {..} = locOf loc

data Declaration
  = VarDecl Variable
  | ConstDecl Constant
  | BusDecl Bus
  | FuncDecl Function
  | EnumDecl Enumeration
  | InstDecl Instance
  | GenDecl Generate
  deriving (Eq, Ord, Show, Data, Typeable)

data Variable = Variable
  { name  :: Ident
  , ty    :: Typeness
  , val   :: Maybe Expr
  , range :: Maybe Range
  , loc   :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Typed Variable where
  typeOf Variable {..} = ty

instance Located Variable where
  locOf Variable {..} = locOf loc

instance Nameable Variable where
  nameOf Variable {..} = name

data Constant = Constant
  { name :: Ident
  , ty   :: Typeness
  , val  :: Expr
  , loc  :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Typed Constant where
  typeOf Constant {..} = ty

instance Located Constant where
  locOf Constant {..} = locOf loc

instance Nameable Constant where
  nameOf Constant {..} = name

data Function = Function
  { name   :: Ident
  , params :: [(Ident, Type)]
  , retTy  :: Type
  , decls  :: [Declaration]
  , body   :: [Statement]
  , loc    :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Typed Function where
  typeOf Function {..} = Typed retTy

instance Located Function where
  locOf Function {..} = locOf loc

instance Nameable Function where
  nameOf Function {..} = name

-- data AssinOp =
--   AssignId
--   | AssignAdd | AssignSub | AssignMinus |

data Statement
  = Assign { dest :: Name
          ,  val  :: Expr
          ,  loc  :: SrcLoc}
  | If { cond :: Expr
      ,  body :: [Statement]
      ,  elif :: [(Expr, [Statement])]
      ,  els  :: Maybe [Statement]
      ,  loc  :: SrcLoc}
  | For { var  :: Ident
       ,  from :: Expr
       ,  to   :: Expr
       ,  body :: [Statement]
       ,  loc  :: SrcLoc}
  | Switch { value       :: Expr
          ,  cases       :: [(Expr, [Statement])]
          ,  defaultCase :: Maybe [Statement]
          ,  loc         :: SrcLoc}
  | Barrier { loc :: SrcLoc}
  | Break { loc :: SrcLoc}
  | Return { retVal :: Maybe Expr
          ,  loc    :: SrcLoc}
  deriving (Eq, Ord, Show, Data, Typeable)

instance Located Statement where
  locOf Assign {..}  = locOf loc
  locOf If {..}      = locOf loc
  locOf For {..}     = locOf loc
  locOf Switch {..}  = locOf loc
  locOf Barrier {..} = locOf loc
  locOf Break {..}   = locOf loc
  locOf Return {..}  = locOf loc

data Enumeration = Enumeration
  { ty     :: Typeness
  , name   :: Ident
  , fields :: [(Ident, Maybe Expr)]
  , loc    :: SrcLoc
  } deriving (Eq, Ord, Show, Data, Typeable)

instance Typed Enumeration where
  typeOf Enumeration {..} = ty

instance Located Enumeration where
  locOf Enumeration {..} = locOf loc

instance Nameable Enumeration where
  nameOf Enumeration {..} = name

data Direction
  = In { loc :: SrcLoc }
  | Out { loc :: SrcLoc }
  | Const { loc :: SrcLoc }
  deriving (Eq, Ord, Show, Data, Typeable)

instance Located Direction where
  locOf In {..}    = locOf loc
  locOf Out {..}   = locOf loc
  locOf Const {..} = locOf loc

data Expr
  = Binary { ty    :: Typeness
           , binOp :: BinOp
           , left  :: Expr
           , right :: Expr
           , loc   :: SrcLoc }
  | Unary { ty    :: Typeness
          , unOp  :: UnOp
          , right :: Expr
          , loc   :: SrcLoc }
  | PrimLit { ty  :: Typeness
            , lit :: Literal
            , loc :: SrcLoc }
  | PrimName { ty   :: Typeness
             , name :: Name
             , loc  :: SrcLoc }
  | FunCall { ty     :: Typeness
            , name   :: Name
            , params :: [Expr]
            , loc    :: SrcLoc }
  deriving (Eq, Ord, Show, Data, Typeable)

instance {-# OVERLAPPING #-} Typed Expr where
  typeOf Binary {..}   = ty
  typeOf Unary {..}    = ty
  typeOf PrimLit {..}  = ty
  typeOf PrimName {..} = ty
  typeOf FunCall {..}  = ty

instance Located Expr where
  locOf Binary {..}   = locOf loc
  locOf Unary {..}    = locOf loc
  locOf PrimLit {..}  = locOf loc
  locOf PrimName {..} = locOf loc
  locOf FunCall {..}  = locOf loc

data BinOp
  =
    -- Arithmetic operators
    DivOp   { loc :: SrcLoc}
  | MinusOp { loc :: SrcLoc}
  | ModOp   { loc :: SrcLoc}
  | MulOp   { loc :: SrcLoc}
  | PlusOp  { loc :: SrcLoc}
  -- Bitwise operators
  | AndOp   { loc :: SrcLoc}
  | OrOp    { loc :: SrcLoc}
  | SllOp   { loc :: SrcLoc}
  | SrlOp   { loc :: SrcLoc}
  | XorOp   { loc :: SrcLoc}
  -- Booelan operators
  | ConOp   { loc :: SrcLoc} -- Conjunction
  | EqOp    { loc :: SrcLoc}
  | DisOp   { loc :: SrcLoc} -- Disjunction
  | GeqOp   { loc :: SrcLoc}
  | GtOp    { loc :: SrcLoc}
  | LeqOp   { loc :: SrcLoc}
  | LtOp    { loc :: SrcLoc}
  | NeqOp   { loc :: SrcLoc}
  deriving (Eq, Ord, Show, Data, Typeable)

instance Located BinOp where
  locOf AndOp   {..} = locOf loc
  locOf ConOp   {..} = locOf loc
  locOf DivOp   {..} = locOf loc
  locOf EqOp    {..} = locOf loc
  locOf DisOp   {..} = locOf loc
  locOf GeqOp   {..} = locOf loc
  locOf GtOp    {..} = locOf loc
  locOf LeqOp   {..} = locOf loc
  locOf LtOp    {..} = locOf loc
  locOf MinusOp {..} = locOf loc
  locOf ModOp   {..} = locOf loc
  locOf MulOp   {..} = locOf loc
  locOf NeqOp   {..} = locOf loc
  locOf OrOp    {..} = locOf loc
  locOf PlusOp  {..} = locOf loc
  locOf SllOp   {..} = locOf loc
  locOf SrlOp   {..} = locOf loc
  locOf XorOp   {..} = locOf loc

data UnOp
  = UnPlus { loc :: SrcLoc }
  | UnMinus { loc :: SrcLoc }
  | NotOp { loc :: SrcLoc }
  | NegOp { loc :: SrcLoc }

  deriving (Eq, Ord, Show, Data, Typeable)

instance Located UnOp where
  locOf UnPlus  {..} = locOf loc
  locOf UnMinus {..} = locOf loc
  locOf NotOp   {..} = locOf loc
  locOf NegOp   {..} = locOf loc

data Name
  = Name { base  :: NamePart
         , parts :: [NamePart]
         , loc   :: SrcLoc
         }
    deriving (Eq, Ord, Show, Data, Typeable)

instance References Name where
  refOf Name {..} = foldl (<>) (refOf base) (map refOf parts)

instance Located Name where
  locOf Name {..}   = locOf loc

data NamePart
  = IdentName { ident :: Ident
              , loc   :: SrcLoc }
  | ArrayAccess { namePart :: NamePart
                , index    :: ArrayIndex
                , loc      :: SrcLoc }
  deriving (Eq, Ord, Show, Data, Typeable)

instance References NamePart where
  refOf IdentName {..}   = ident N.:| []
  refOf ArrayAccess {..} = refOf namePart

instance Located NamePart where
  locOf IdentName {..}   = locOf loc
  locOf ArrayAccess {..} = locOf loc

data ArrayIndex
  = Wildcard
  | Index Expr
  deriving (Eq, Ord, Show, Data, Typeable)

data Type
  = Signed { size :: Maybe Integer
           , loc  :: SrcLoc }
  | Unsigned { size :: Maybe Integer
             , loc  :: SrcLoc }
  | Single { loc :: SrcLoc }
  | Double { loc :: SrcLoc }
  | Bool { loc :: SrcLoc }
  | String { loc :: SrcLoc }
  | Array { arrLength :: Maybe Expr
          , innerTy   :: Type
          , loc       :: SrcLoc }
  deriving (Eq, Ord, Show, Data, Typeable)

-- instance Ord Type where
--   compare

instance Located Type where
  locOf Signed {..}   = locOf loc
  locOf Unsigned {..} = locOf loc
  locOf Single {..}   = locOf loc
  locOf Double {..}   = locOf loc
  locOf Bool {..}     = locOf loc
  locOf String {..}   = locOf loc
  locOf Array {..}    = locOf loc

data Literal
  = LitInt { intVal :: Integer
           , loc    :: SrcLoc }
  | LitFloat { floatVal :: Double
             , loc      :: SrcLoc }
  | LitString { stringVal :: T.Text
              , loc       :: SrcLoc }
  | LitArray { arrayVal :: [Integer]
             , loc      :: SrcLoc }
  -- TODO: Temporary limitation
  -- LitArray { arrayVal :: [Expr]
  --            , loc      :: SrcLoc }
  | LitTrue { loc :: SrcLoc }
  | LitFalse { loc :: SrcLoc }
  deriving (Eq, Ord, Show, Data, Typeable)

instance Located Literal where
  locOf LitInt {..}    = locOf loc
  locOf LitFloat {..}  = locOf loc
  locOf LitString {..} = locOf loc
  locOf LitArray {..}  = locOf loc
  locOf LitTrue {..}   = locOf loc
  locOf LitFalse {..}  = locOf loc

-- | Returns the minimum number of bits required to represent a number
bitSize :: forall a. (Integral a) => a -> a
bitSize = (+ 1) . (floor :: Double -> a) . logBase 2 . fromIntegral . abs

instance Typed Literal where
  typeOf LitInt {..}
    | intVal < 0 =
      let res = Typed $ Signed (Just (bitSize intVal)) loc
      in trace ("Typeof for " ++ show intVal ++ " yielded " ++ show res) res
    | intVal == 0 =
      Typed $ Unsigned (Just 1) loc
    | otherwise =
      let res = Typed $ Unsigned (Just (bitSize intVal)) loc
      in trace ("Typeof for " ++ show intVal ++ " yielded " ++ show res) res
  typeOf LitFloat {..} = Typed $ Single loc
  typeOf LitString {..} = Typed $ String loc
  -- TODO: Do better! This requires a pre-processing step simplifying the
  -- expression comprising the array literal
  typeOf LitArray {..} =
    Typed $
    Array
      (Just
         (PrimLit Untyped (LitInt (fromIntegral $ length arrayVal) noLoc) noLoc))
      arrTy
      loc
    where
      arrTy
        | minimum arrayVal < 0 =
          Signed (Just (bitSize $ maximum (map abs arrayVal))) loc
        | otherwise = Unsigned (Just (bitSize $ maximum arrayVal)) loc
  typeOf LitTrue {..} = Typed $ Bool loc
  typeOf LitFalse {..} = Typed $ Bool loc

data Ident = Ident
  { val :: T.Text
  , loc :: SrcLoc
  } deriving (Show, Data, Typeable)

instance Eq Ident where
  -- Disregard locations when comparing identities
  -- TODO: Maybe an Eq instance for Ident is better?
  Ident { val = val1 } == Ident { val = val2 } = val1 == val2

instance Ord Ident where
  Ident {val = val1 } `compare` Ident {val = val2} = val1 `compare` val2

instance References Ident where
  refOf i = i N.:| []

instance Located Ident where
  locOf Ident {..} = locOf loc

instance ToString Ident where
  toString (Ident v _) = T.unpack v
  toText (Ident v _)  = v

instance Hashable Ident where
  hashWithSalt s Ident {..} = hashWithSalt s val

instance Nameable Ident where
  nameOf = id

instance Semigroup Ident where
  (Ident a loca) <> (Ident b locb) = Ident (a <> b) (loca <> locb)

-- Instances for standard number types
instance {-# OVERLAPPABLE #-} (Integral a) => Typed a where
  typeOf intVal
    | intVal < 0 = Typed $ Signed (Just (fromIntegral $ bitSize intVal)) noLoc
    | otherwise = Typed $ Unsigned (Just (fromIntegral $ bitSize intVal)) noLoc

instance Typed Double where
  typeOf _ = Typed $ Double noLoc

instance Typed Float where
  typeOf _ = Typed $ Double noLoc
