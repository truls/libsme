module SME.Util
  ( displayLoc'
  , fst3
  , mkRange
  ) where

import           Data.Loc              (Located, displayLoc, locOf, noLoc)

import           Language.SMEIL.Syntax

displayLoc' :: (Located a) => a -> String
displayLoc' = displayLoc . locOf

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- TODO: This will go awfully wrong if the given literal is somehow not an
-- int. Make sure this cant happen
mkRange :: Maybe Literal -> Maybe Range
mkRange =
  fmap
    (\x ->
       Range
         (PrimLit (typeOf (0 :: Integer)) (LitInt 0 noLoc) noLoc)
         (PrimLit (typeOf x) x noLoc)
         noLoc)
