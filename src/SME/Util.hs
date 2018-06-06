module SME.Util
  ( displayLoc'
  , mkRange
  , identToName
  ) where

import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Data.Loc              (Located, displayLoc, locOf, noLoc)

import           Language.SMEIL.Syntax

displayLoc' :: (Located a) => a -> String
displayLoc' = displayLoc . locOf

mkRange :: Maybe (Literal, Literal) -> Maybe Range
mkRange =
  fmap
    (\(l, u) ->
       Range
         (PrimLit (typeOf l) l noLoc)
         (PrimLit (typeOf u) u noLoc)
         noLoc)

-- FIXME: Consider a type class for this
identToName :: Ident -> Name
identToName i = Name (IdentName i noLoc :| []) noLoc
