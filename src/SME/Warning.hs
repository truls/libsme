{-# LANGUAGE GADTs #-}

module SME.Warning
  ( TypeCheckWarning(..)
  ) where

import           Data.Loc

--import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax (ToString (..))

data TypeCheckWarning where
  UnusedVariable :: (ToString a, Located a) => a -> TypeCheckWarning
  UnassignedBus :: (ToString a, Located a) => a -> TypeCheckWarning

instance Show TypeCheckWarning where
  show (UnusedVariable var) =
    "Unused variable " ++ toString var ++ " defined at " ++ displayLoc' var
  show (UnassignedBus _) = ""

displayLoc' :: (Located a) => a -> String
displayLoc' = displayLoc . locOf
