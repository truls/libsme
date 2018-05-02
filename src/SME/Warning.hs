{-# LANGUAGE GADTs #-}

module SME.Warning
  ( TypeCheckWarning(..)
  , Warns
  ) where

import           Data.Loc

import           Language.SMEIL.Pretty
import           Language.SMEIL.Syntax

type Warns = [TypeCheckWarning]

data TypeCheckWarning where
  UnusedVariable :: (ToString a, Located a) => a -> TypeCheckWarning
  UnusedConstant :: (ToString a, Located a) => a -> TypeCheckWarning
  UnassignedBus :: (ToString a, Located a) => a -> TypeCheckWarning
  BoundedVarForConst
    :: (Nameable a, Located a) => a -> Typeness -> TypeCheckWarning

instance Show TypeCheckWarning where
  show (UnusedVariable v) =
    "Unused variable " ++ toString v ++ " defined at " ++ displayLoc' v
  show (UnusedConstant v) =
    "Unused constant " ++ toString v ++ " defined at " ++ displayLoc' v
  show (UnassignedBus _) = ""
  show (BoundedVarForConst a t) =
    "Bounded type " ++
    pprrString t ++
    " used for constant " ++
    toString (nameOf a) ++
    ". This is (almost) never what you want. At " ++ displayLoc' a

displayLoc' :: (Located a) => a -> String
displayLoc' = displayLoc . locOf
