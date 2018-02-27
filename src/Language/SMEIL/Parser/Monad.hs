-- | Contains the parser monad definition and related operations

module Language.SMEIL.Parser.Monad
  ( Parser
  , makePos
  , makePos'
  , newPos
  , putPos
  , withPos
  ) where

import           Control.Monad.State
import           Data.Loc
import qualified Data.Text           as T
import           Data.Void
import           Text.Megaparsec     hiding (Pos)

-- | Monad that our parser lives in
type Parser = StateT Pos (Parsec Void T.Text)

-- TODO: This code is somewhat broken. Probably better expressed using a reader
-- monad, but we can also change the state to be a stack.
newPos :: Pos
newPos = linePos "(nofile)" 0

toPos :: SourcePos -> Pos
toPos (SourcePos fp line column) = Pos fp (unPos line) (unPos column) 0

getPos :: Parser Pos
getPos = toPos <$> getPosition

-- | Tracks the positions around a parser and applies the location to the result
-- of parser 'p'. @withPos (many 'a')@ is the equivalent of @do { putPos;
-- makePos (many 'a') }@
withPos :: Parser (SrcLoc -> a) -> Parser a
withPos p = do
  pos <- getPos
  res <- p
  retSpan pos res

-- | Save the current position
putPos :: Parser ()
putPos = do
  pos <- getPos
  put pos

-- | Creates a span based on saved position and current position and applies it
-- to the result of 'p'
makePos :: Parser (SrcLoc -> a) -> Parser a
makePos p = do
  res <- p
  pos <- get
  retSpan pos res

-- | Returns a span between saved position and current position
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
