{-# LANGUAGE OverloadedStrings #-}

module Language.SMEIL.Parser.Impl where

import           Control.Monad               (void)
import           Data.List.NonEmpty          (NonEmpty ((:|)), fromList)
import           Data.Loc                    (SrcLoc)
import           Data.Maybe                  (fromMaybe, isJust)
import qualified Data.Text                   as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm

import           Language.SMEIL.Parser.Lexer
import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

-- Top level constructs

designFile :: Parser S.DesignFile
designFile = withPos $ spaceConsumer >> S.DesignFile <$> some designUnit <* eof

designUnit :: Parser S.DesignUnit
designUnit = withPos $ S.DesignUnit <$> many importStm <*> some unitElement

unitElement :: Parser S.UnitElement
unitElement = choice [S.UnitProc <$> process, S.UnitNet <$> network]

-- Network Structure

importStm :: Parser S.Import
importStm =
  withPos
    (choice
       [ reserved "import" >> S.SimpleImport <$> modName <*> qualified <* semi
       , reserved "from" >>
         S.SpecificImport <$> (modName <* reserved "import") <*>
         (ident `sepBy1` comma) <*>
         qualified <*
         semi
       ])
  where
    modName = do
      first <- ident
      rest <- optional $ dot *> ident `sepBy` dot
      return (first :| fromMaybe [] rest)
    qualified = optional (reserved "as" *> ident)

network :: Parser S.Network
network =
  withPos
    (reserved "network" >>
     S.Network <$> ident <*> parens (param `sepBy` comma) <*>
     braces (some networkDecl))

networkDecl :: Parser S.NetworkDecl
networkDecl = choice [S.NetInst <$> instanceDecl
                     , S.NetBus <$> busDecl
                     , S.NetConst <$> constDecl
                     , S.NetGen <$> genDecl
                     ]

process :: Parser S.Process
process =
  withPos $ do
    sync <- synchrony
    void $ reserved "proc"
    S.Process <$> ident <*> parens (param `sepBy` comma) <*> many declaration <*>
      braces (many statement) <*>
      pure sync
  where
    synchrony =
      reserved "sync" *> pure True <|> reserved "async" *> pure False <|>
      pure False

param :: Parser S.Param
param =
  withPos $
  S.Param <$> optional (brackets (optional expression)) <*> direction <*> ident

-- Definitions
instanceDecl :: Parser S.Instance
instanceDecl =
  withPos
    (reserved "instance" >>
     S.Instance <$> (transformIdent <$> ident) <*>
     (optional (brackets expression) <* reserved "of") <*>
     name <*>
     parens (paramMap `sepBy` comma) <*
     semi)
  where
    paramMap = (,) <$> optional (try (ident <* colon)) <*> expression
    transformIdent (S.Ident "_" _) = Nothing
    transformIdent i@S.Ident {}    = Just i

enum :: Parser S.Enumeration
enum =
  withPos $
  reserved "enum" >>
  S.Enumeration S.Untyped <$> ident <*>
  braces (fromList <$> enumField `sepBy1` comma) <*
  semi
  where
    enumField = (,) <$> ident <*> optional (symbol "=" *> expression)

busDecl :: Parser S.Bus
busDecl =
  withPos
    (busProps <*> (reserved "bus" *> ident) <*>
     braces (some (withPos signalDecl)) <*
     semi <?> "bus declaration")
  where
    signalDecl =
      S.BusSignal <$> (ident <* colon) <*> (S.Typed <$> typeName) <*>
      optional (symbol "=" *> expression) <*>
      optional range <*
      semi <?> "bus signal declaration"
    busProps =
      makePermParser $
      S.Bus <$?> (False, reserved "exposed" *> pure True) <|?>
      (False, reserved "unique" *> pure True)

varDecl :: Parser S.Variable
varDecl =
  withPos
    (reserved "var" >>
     S.Variable <$> (ident <* colon) <*> (S.Typed <$> typeName) <*>
     optional (symbol "=" *> expression) <*>
     optional range <*
     semi <?> "variable declaration")

genDecl :: Parser S.Generate
genDecl =
  withPos
    (reserved "generate" >>
     S.Generate <$> (ident <* equal) <*> (expression <* reserved "to") <*>
     expression <*>
     braces (many networkDecl)) <?>
  "generate declaration"

range :: Parser S.Range
range =
  withPos
    (reserved "range" >>
     S.Range <$> expression <* reserved "to" <*>
     expression <?> "range constraint")

constDecl :: Parser S.Constant
constDecl =
  withPos
    (reserved "const" >>
     S.Constant <$> (ident <* colon) <*> (S.Typed <$> typeName) <*>
     (symbol "=" *> expression) <*
     semi <?> "constant declaration")

function :: Parser S.Function
function =
  withPos
    (reserved "func" >>
     S.Function <$> ident <*> parens (funcParam `sepBy` comma) <*>
     (colon *> typeName) <*>
     many declaration <*>
     braces (many statement) <*
     semi <?> "function")
  where
    funcParam = (,) <$> (ident <* colon) <*> typeName

declaration :: Parser S.Declaration
declaration =
  choice
    [ S.VarDecl <$> varDecl
    , S.ConstDecl <$> constDecl
    , S.BusDecl <$> busDecl
    , S.FuncDecl <$> function
    , S.EnumDecl <$> enum
    , S.InstDecl <$> instanceDecl
    , S.GenDecl <$> genDecl
    ] <?>
  "declaration"

---------------------------------------------------------------------------------
-- Statements
---------------------------------------------------------------------------------

statement :: Parser S.Statement
statement =
  withPos
    (ifStm <|> forStm <|> switchStm <|> barrierStm <|> breakStm <|> traceStm <|>
     returnStm <|>
     assignStm <?> "statement")
  where
    assignStm = S.Assign <$> (name <* equal) <*> expression <* semi
    ifStm =
      reserved "if" >>
      S.If <$> parens expression <*> braces (some statement) <*>
      many
        (reserved "elif" *>
         ((,) <$> parens expression <*> braces (many statement))) <*>
      optional (reserved "else" *> braces (many statement)) <?> "if statement"
    forStm =
      reserved "for" >>
      S.For <$> (ident <* equal) <*> (expression <* reserved "to") <*>
      expression <*>
      braces (many statement) <?> "for statement"
    switchStm =
      reserved "switch" >>
      S.Switch <$> expression <*>
      (symbol "{" *> many switchCase) <*>
      (optional defaultCase <* symbol "}") <?> "switch statement"
      where
        switchCase =
          reserved "case" >>
          (,) <$> expression <*> braces (some statement) <?> "switch case"
        defaultCase = reserved "default" *> braces (some statement)
    traceStm =
      reserved "trace" >>
      (parens
        -- TODO: Make the stringLit parser return a Literal type
         (S.Trace <$> (withPos (S.LitString <$> stringLit) <* comma) <*>
          (expression `sepBy` comma)) <*
       semi)
    barrierStm = reserved "barrier" >> semi >> pure S.Barrier
    breakStm = reserved "break" >> semi >> pure S.Break
    returnStm = reserved "return" >> S.Return <$> optional expression <* semi

name :: Parser S.Name
name = withPos $ do
  rest <- namePart `sepBy1` dot
  return $ S.Name $ fromList rest

namePart :: Parser S.NamePart
namePart = do
  putPos
  ident' <- makePos (S.IdentName <$> ident)
  choice [makePos $ S.ArrayAccess ident' <$> brackets arrayIndex, pure ident']

arrayIndex :: Parser S.ArrayIndex
arrayIndex = (symbol "*" >> pure S.Wildcard) <|> S.Index <$> expression

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

expression :: Parser S.Expr
expression = makeExprParser term table <?> "expression"

parensExpr :: Parser S.Expr
parensExpr = withPos (S.Parens S.Untyped <$> expression)

term :: Parser S.Expr
term =
  choice
    [ parens parensExpr
    , withPos $ S.PrimLit S.Untyped <$> (literal <|> withPos arrayLit)
    , try (withPos funCall)
    , withPos $ S.PrimName S.Untyped <$> name
    ] <?>
  "term"
  where
    funCall =
      S.FunCall S.Untyped <$> name <*> parens (expression `sepBy1` comma)
    arrayLit = S.LitArray <$> brackets (integer `sepBy` comma)
    -- TODO: Temporary limitation
    --arrayLit = S.LitArray <$> brackets (expression `sepBy` comma)

table :: [[Operator Parser S.Expr]]
table =
  let unary = S.Unary S.Untyped
      bin = S.Binary S.Untyped
  in [ [ prefix "+" unary S.UnPlus
       , prefix "-" unary S.UnMinus
       , prefix "!" unary S.NotOp
       , prefix "~" unary S.NegOp
       ]
     , [binary "*" bin S.MulOp, binary "/" bin S.DivOp, binary "%" bin S.ModOp]
     , [binary "+" bin S.PlusOp, binary "-" bin S.MinusOp]
     , [binary "<<" bin S.SllOp, binary ">>" bin S.SrlOp]
     , [ binary ">=" bin S.GeqOp
       , binary "<=" bin S.LeqOp
       , binary "<" bin S.LtOp
       , binary ">" bin S.GtOp
       ]
     , [binary "==" bin S.EqOp, binary "!=" bin S.EqOp]
     , [ binary' (doubleSymGuard '&') bin S.AndOp
       , binary "^" bin S.XorOp
       , binary' (doubleSymGuard '|') bin S.OrOp
       ]
     , [binary "&&" bin S.ConOp]
     , [binary "||" bin S.DisOp]
     ]

doubleSymGuard :: Char -> Parser ()
doubleSymGuard c = try $ lexeme $ char c >> notFollowedBy (char c)

posSymbol :: Parser a -> (SrcLoc -> b) -> Parser (b, SrcLoc)
posSymbol p f = do
  putPos
  _ <- p
  pos <- makePos'
  return (f pos, pos)

binary' ::
  Parser a
  -> (b -> c -> c -> SrcLoc -> c)
  -> (SrcLoc -> b)
  -> Operator Parser c
binary' p f g =
  InfixL
    (do (g', pos) <- posSymbol p g
        return (\s r -> f g' s r pos))

binary ::
     T.Text
  -> (a -> b -> b -> SrcLoc -> b)
  -> (SrcLoc -> a)
  -> Operator Parser b
binary n =
  binary' (symbol n)

prefix ::
     T.Text -> (a -> b -> SrcLoc -> b) -> (SrcLoc -> a) -> Operator Parser b
prefix n f g =
  Prefix
    (do (g', pos) <- posSymbol (symbol n) g
        return (\s -> f g' s pos))

typeName :: Parser S.Type
typeName =
  withPos
    (choice
       [ try $ char 'i' >> S.Signed . Just <$> integer
       , symbol "int" >> pure (S.Signed Nothing)
       , try $ char 'u' >> S.Unsigned . Just <$> integer
       , symbol "uint" >> pure (S.Unsigned Nothing)
       , char 'f' >>
         ((string "32" >> pure S.Single) <|> (string "64" >> pure S.Double))
       , symbol "bool" >> pure S.Bool
       , S.Array <$> brackets (optional expression) <*> typeName
       ])

-- Utility Functions

parses :: Parser a -> Parser Bool
parses p = isJust <$> optional p
