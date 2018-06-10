module Main where

import           Control.Exception.Safe (throw)
import           Control.Monad          (unless, when)
import           Options.Applicative
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist)

import           SME.Error
import           SME.Stages

optsParser :: ParserInfo Config
optsParser =
  info
    (cmdLineOptParser <**> helper)
    (fullDesc <> progDesc "SMEIL to VHDL compiler" <>
     header "smec -- SME compiler")

main :: IO ()
main = do
  opts <- execParser optsParser
  doesFileExist (inputFile opts) >>=
    flip unless (throw $ FileNotFound $ inputFile opts)
  case outputDir opts of
    Just d -> do
      exists <- doesDirectoryExist d
      when (exists && not (force opts)) (throw (DirAlreadyExists d))
      unless exists $ createDirectory d
    Nothing -> return ()
  exitOnError $ compile opts
