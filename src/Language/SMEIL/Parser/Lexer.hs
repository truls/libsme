{-# LANGUAGE OverloadedStrings #-}

module Language.SMEIL.Parser.Lexer
  ( lexeme
  , symbol
  , ident
  , reserved
  , literal
  , parens
  , braces
  , brackets
  , comma
  , colon
  , dot
  , semi
  , integer
  , stringLit
  , spaceConsumer
  , equal
  , direction
  ) where

import           Control.Applicative         (empty)
import           Control.Monad               (when)
import           Data.Char                   (isLatin1)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Language.SMEIL.Parser.Monad
import qualified Language.SMEIL.Syntax       as S

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")

semi, comma, colon, dot, equal :: Parser T.Text
semi  = symbol ";"
comma = symbol ","
colon = symbol ":"
dot   = symbol "."
equal   = symbol "="

reserved :: T.Text -> Parser T.Text
reserved w = do
  r <- lexeme (string w)
  when (r `notElem` reservedWords) $ fail (T.unpack r <> " is not a reserved word")
  return r

ident :: Parser S.Ident
ident = lexeme $ withPos $ do
    i <- part <|> string "_" <* notFollowedBy part
    when (i `elem` reservedWords) $
      fail $ "Keyword " ++ T.unpack i ++ " used as identifier"
    return (S.Ident i)
  where
    part = T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

integer :: Parser Integer
integer = lexeme $ L.signed spaceConsumer (hex <|> oct <|> dec)
  where
    hex = try (string "0x") >> L.hexadecimal
    oct = try (string "0o") >> L.octal
    dec = L.decimal

float :: Parser Double
float = lexeme $ L.signed spaceConsumer L.float

stringLit :: Parser T.Text
stringLit =
  T.pack <$>
  lexeme
    (between (char '"') (char '"' <?> "end of string") (many strChar))
  where
    strChar =
      satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026') && isLatin1 c) <|>
      (char '\\' >> char '"')

literal :: Parser S.Literal
literal =
  withPos $ choice
    [ symbol "true" >> pure S.LitTrue
    , symbol "false" >> pure S.LitFalse
    , symbol "'U" >> pure S.LitUndef
    , S.LitString <$> stringLit
    , try $ S.LitFloat <$> float
    , S.LitInt <$> integer
    ]

direction :: Parser S.Direction
direction =
  withPos $ choice
    [ reserved "in"    >> pure S.In
    , reserved "out"   >> pure S.Out
    , reserved "const" >> pure S.Const
    ]

reservedWords :: [T.Text]
reservedWords =
  [ "as"
  , "assert"
  , "async"
  , "barrier"
  , "break"
  , "bus"
  , "case"
  , "const"
  , "default"
  , "elif"
  , "else"
  , "enum"
  , "exposed"
  , "for"
  , "from"
  , "func"
  , "generate"
  , "if"
  , "import"
  , "in"
  , "instance"
  , "network"
  , "of"
  , "out"
  , "proc"
  , "range"
  , "return"
  , "switch"
  , "sync"
  , "to"
  , "trace"
  , "unique"
  , "var"
  ]
