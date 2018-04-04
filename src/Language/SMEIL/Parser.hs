module Language.SMEIL.Parser
  ( parse
  , parserTest
  ) where


import           Control.Monad.State         (runStateT)
import qualified Data.Text                   as T
import qualified Text.Megaparsec             as P

import           Language.SMEIL.Syntax

import           Language.SMEIL.Parser.Impl
import           Language.SMEIL.Parser.Monad

parse :: String -> T.Text -> Either String DesignFile
parse f c =
  case P.runParser (runStateT designFile newPos) f c of
    Left err     -> Left $ P.parseErrorPretty err
    Right (r, _) -> Right r

parserTest :: Parser a -> T.Text -> Either String a
parserTest p c =
  let f = "(test)"
  in case P.runParser (runStateT p newPos) f c of
       Left err     -> Left $ P.parseErrorPretty err
       Right (r, _) -> Right r
