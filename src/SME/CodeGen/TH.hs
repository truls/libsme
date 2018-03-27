{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module SME.CodeGen.TH (parseVHDLToQexp) where

import           Data.Data                  (Data, cast)
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax

import           Language.VHDL

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)

parseVHDLToQexp :: FilePath -> Q Exp
parseVHDLToQexp fp = do
  addDependentFile fp
  res <- runIO $ parseFile fp >>= \case
    Left e  -> fail (show e)
    Right r -> pure r
  liftDataWithText res
