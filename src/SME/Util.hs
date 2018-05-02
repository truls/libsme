module SME.Util
  ( displayLoc'
  ) where

import           Data.Loc (Located, displayLoc, locOf)

displayLoc' :: (Located a) => a -> String
displayLoc' = displayLoc . locOf
