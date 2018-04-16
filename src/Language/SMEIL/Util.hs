{-# LANGUAGE DuplicateRecordFields #-}

module Language.SMEIL.Util
  ( setTypeLoc
  ) where

import           Data.Loc              (Loc, fromLoc)
import           Language.SMEIL.Syntax (Type (..), Typeness (..))

-- | Sets the location of type
setTypeLoc :: Loc -> Typeness -> Typeness
setTypeLoc _ Untyped   = Untyped
setTypeLoc l (Typed t) = Typed (t { loc = fromLoc l } :: Type)
