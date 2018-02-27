module SME.CodeGen
  ( genOutput
  , Language(..)
  ) where

import           SME.CodeGen.Common
import           SME.CodeGen.CXX
import           SME.CodeGen.Python
import           SME.CodeGen.VHDL
import           SME.Representation

data Language
  = VHDL
  | CXX
  | Python

genOutput :: Language -> a
genOutput VHDL   = genVHDL
genOutput CXX    = genCxx
genOutput Python = genPython
