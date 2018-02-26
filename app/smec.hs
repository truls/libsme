module Main where

import           Control.Exception   (throwIO)
import           Control.Monad       (unless, when)
import           Data.List           (intercalate, nub)
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Directory    (createDirectory, doesDirectoryExist,
                                      doesFileExist)

import           SME.Error
import           SME.Stages

optParser :: Parser Config
optParser =
  Config <$>
  strOption
    (long "input" <> metavar "IN" <> short 'i' <>
     help "Input file. Specify - for stdin.") <*>
  strOption
    (long "output" <> metavar "OUT" <> short 'o' <> value "output" <>
     help "Output directory. Defaults to output.") <*>
  (nub <$>
   many
     (option
        auto
        (long "dump-stage" <>
         help
           ("For debugging. Dumps the state of the compiler resulting from stage. Can be specified multiple times with one of the following arguments" ++
            stagesPP)))) <*>
  switch
    (long "force" <> short 'f' <>
     help "Allow existing output directories and overwrite files") <*>
  switch
    (long "strict-size-bounds" <> help "Strictly enforce size bounds of types") <*>
  switch
    (long "infer-size-bounds" <>
     help "Adjust size bounds in code to inferred values") <*>
  switch (long "no-warnings" <> short 'w' <> help "Disable warnings")
  where
    stagesPP =
      intercalate ", " (map show [ResolveImport, TypeCheck, Optimize]) ++
      " or " ++ show CodeGen

optsParser :: ParserInfo Config
optsParser =
  info
    (optParser <**> helper)
    (fullDesc <> progDesc "SMEIL to VHDL compiler" <>
     header "smec -- SME compiler")

main :: IO ()
main = do
  opts <- execParser optsParser
  doesFileExist (inputFile opts) >>= flip unless (throwIO $ FileNotFound $ inputFile opts)
  exists <- doesDirectoryExist (outputDir opts)
  when (exists && not (force opts)) (throwIO (DirAlreadyExists (outputDir opts)))
  unless exists $ createDirectory (outputDir opts)
  compile opts
