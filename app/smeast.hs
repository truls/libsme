--{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception     (throwIO)
import           Control.Monad         (unless)
import           Data.Char             (isLetter, toLower)
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Options.Applicative   hiding (value)
import qualified Options.Applicative   as O (value)
import           System.Directory      (doesFileExist)
import           Text.Show.Pretty      (ppShow)

import           Language.SMEIL.Parser (parse)
import           Language.SMEIL.Pretty (pprr)
-- import           Language.SMEIL.JSON
-- import           Language.SMEIL.Syntax

data Format
  = PrettyJSON
  | JSON
  | Pretty
  | AST

instance Read Format where
  readsPrec _ input =
    let (tok, rest) = span (\l -> isLetter l || l == '-')  input
    in case mapFormat tok of
         Just f  -> [(f, rest)]
         Nothing -> []
    where
      mapFormat =
        (`lookup` [ ("pretty-json", PrettyJSON)
                  , ("json", JSON)
                  , ("pretty", Pretty)
                  , ("ast", AST)
                  ]) .
        map toLower

instance Show Format where
  show PrettyJSON = "pretty-json"
  show JSON       = "json"
  show Pretty     = "pretty"
  show AST        = "ast"

data Options = Options
  { inputFile    :: FilePath
  , outputFile   :: FilePath
  , inputFormat  :: Format
  , outputFormat :: Format
  }

optParser :: Parser Options
optParser =
  Options <$>
  strOption
    (long "input" <> metavar "IN" <> short 'i' <> O.value "-" <>
     help "Input file. Defaults to stdin.") <*>
  strOption
    (long "output" <> metavar "OUT" <> short 'o' <> O.value "-" <>
     help "Output file. Defaults to stdout") <*>
  option
    auto
    (long "input-format" <> short 'f' <>
     help ("Format of input file. ARG must be one of choices " ++ formats)) <*>
  option
    auto
    (long "output-format" <> short 'g' <>
     help ("Format of output file. ARG must be one of choices " ++ formats))
  where
    formats =
      show PrettyJSON ++
      ", " ++ show JSON ++ ", " ++ show Pretty ++ " or " ++ show AST

opts :: ParserInfo Options
opts =
  info
    (optParser <**> helper)
    (fullDesc <> progDesc "Converts between different representations of SMEIL" <>
     header "smeast - SMEIL representation converter")

raiseEither :: Either String b -> IO b
raiseEither (Right r) = pure r
raiseEither (Left l)  = throwIO $ userError l

prettyStdin :: String -> String
prettyStdin "-" = "(stdin)"
prettyStdin s   = s

main :: IO ()
main = do
  o <- execParser opts
  let inf = inputFile o
  let ouf = outputFile o
  fc <-
    case inf of
      "-" -> TIO.getContents
      _ -> do
        doesFileExist inf >>=
          flip unless (throwIO (userError $ "Input file not found " ++ inf))
        TIO.readFile inf
  ast <-
    case inputFormat o of
      PrettyJSON -> undefined -- raiseEither (readJSON fc :: Either String (DesignFile SrcSpan))
      JSON       -> undefined -- raiseEither (readJSON fc :: Either String (DesignFile SrcSpan))
      Pretty     -> raiseEither $ parse (prettyStdin inf) fc
      AST        -> throwIO (userError
                             "Pretty printed AST cannot be used as source format")
  let out =
        case outputFormat o of
          PrettyJSON -> undefined -- genJSONPretty ast
          JSON       -> undefined -- genJSON ast
          Pretty     -> pprr ast
          AST        -> T.pack $ ppShow ast
  if ouf == "-"
    then TIO.putStrLn out
    else TIO.writeFile ouf out
