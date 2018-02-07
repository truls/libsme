-- | Contains the parser monad definition and related operations

module Language.SMEIL.Parser.Monad
  ( Parser
  , makePos
  , makePos'
  , newPos
  , putPos
  , withPos
  ) where

import           Control.Monad.State.Lazy
import           Data.Loc
import           Data.Void
import           Text.Megaparsec          hiding (Pos)

type Parser = StateT Pos (Parsec Void String)

newPos :: Pos
newPos = linePos "(nofile)" 0

toPos :: SourcePos -> Pos
toPos (SourcePos fp line column) = Pos fp (unPos line) (unPos column) 0

getPos :: Parser Pos
getPos = toPos <$> getPosition

withPos :: Parser (SrcLoc -> a) -> Parser a
withPos p = do
  pos <- getPos
  res <- p
  retSpan pos res

putPos :: Parser ()
putPos = do
  pos <- getPos
  put pos

makePos :: Parser (SrcLoc -> a) -> Parser a
makePos p = do
  res <- p
  pos <- get
  retSpan pos res

makePos' :: Parser SrcLoc
makePos' = do
  pos <- get
  pos' <- getPos
  return $ fromLoc $ Loc pos pos'

retSpan :: Pos -> (SrcLoc -> a) -> Parser a
retSpan pos res = do
  pos' <- getPos
  let srcSpan = fromLoc $ Loc pos pos'
  return (res srcSpan)
