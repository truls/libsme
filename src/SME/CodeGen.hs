{-# LANGUAGE RecordWildCards #-}

module SME.CodeGen
  ( genOutput
  , Language(..)
  ) where

import           Control.Exception  (throw, throwIO)
import           Control.Monad      (forM_, unless, when)
import qualified Data.Text.IO       as TIO
import           System.Directory   (createDirectory, doesDirectoryExist)
import           System.FilePath    ((</>))

import           SME.CodeGen.Common
import           SME.CodeGen.CXX
import           SME.CodeGen.Python
import           SME.CodeGen.VHDL
import           SME.Error
import           SME.Representation


data Language
  = VHDL
  | CXX
  | Python

writeOutput :: FilePath -> OutputFile -> IO ()
writeOutput dir f@OutputFile {..} =
  TIO.writeFile (dir </> fileName f) content

genOutput' :: Language -> GenM OutputPlan
genOutput' VHDL   = genVHDL
genOutput' CXX    = genCxx
genOutput' Python = genPython

genOutput :: FilePath -> Language -> Env -> IO ()
genOutput destdir lang env = do
  let (res, _) = runGenM env (genOutput' lang)
  let forceMkDir = (force . config) env
  exists <- doesDirectoryExist destdir
  when (exists && not forceMkDir) (throwIO (DirAlreadyExists destdir))
  unless exists $ createDirectory destdir
  plan <-
    case res of
      Left e  -> throw e
      Right r -> pure r
  forM_ plan $ writeOutput destdir
  return ()
