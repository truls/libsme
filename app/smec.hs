module Main where

import           Control.Exception   (catch, throwIO)
import           Control.Monad       (unless, when)
import           Options.Applicative
import           System.Directory    (createDirectory, doesDirectoryExist,
                                      doesFileExist)
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)

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
    flip unless (throwIO $ FileNotFound $ inputFile opts)
  case outputDir opts of
    Just d -> do
      exists <- doesDirectoryExist d
      when (exists && not (force opts)) (throwIO (DirAlreadyExists d))
      unless exists $ createDirectory d
    Nothing -> return ()
  compile opts `catch`
    (\(CompilerError e) -> do
       hPutStrLn stderr e
       exitFailure)
