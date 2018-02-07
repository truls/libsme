-- | TH derived Data.Aeson instances for "Language.SMEIL.Syntax"

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.SMEIL.JSON
  (
  --   readJSON
  -- , genJSON
  -- , genJSONPretty
  ) where

-- import           Language.SMEIL.Parser
-- import           Language.SMEIL.Syntax

-- import           Control.Monad            (mapM)
-- import           Data.Aeson               (FromJSON, ToJSON, eitherDecode,
--                                            encode)
-- import           Data.Aeson.Encode.Pretty (encodePretty)
-- import           Data.Aeson.TH            (Options (..), SumEncoding (..),
--                                            defaultOptions, deriveJSON,
--                                            sumEncoding)
-- import           Data.ByteString.Lazy     (ByteString)

-- import           Data.Char                (isUpper, toLower)
-- import           Data.List                (intercalate)
-- import           Data.List.Split          (keepDelimsL, split, whenElt)
-- import           Text.Megaparsec.Pos

-- -- | Reads a bytestring of JSON and returns a SMEIL AST fragment
-- readJSON
--   :: (FromJSON a)
--   => ByteString -> Either String a
-- readJSON = eitherDecode

-- -- | Transforms a SMEIL AST to a JSON bytestring
-- genJSON
--   :: (ToJSON a)
--   => a -> ByteString
-- genJSON = encode

-- -- | Transforms a SMEIL AST to a pretty JSON bytestrinq
-- genJSONPretty
--   :: (ToJSON a)
--   => a -> ByteString
-- genJSONPretty = encodePretty

-- concat <$>
--   mapM
--     (deriveJSON
--        (let
--            jsonName s =
--              intercalate "-" $
--              filter (not . null) $
--              map (map toLower) ((split . keepDelimsL . whenElt) isUpper s)
--         in
--            defaultOptions
--            { sumEncoding = ObjectWithSingleField
--            , tagSingleConstructors = True
--            , constructorTagModifier = jsonName
--            , fieldLabelModifier = jsonName
--            }))
--     [ ''DesignFile
--     , ''DesignUnit
--     , ''UnitElement
--     , ''Import
--     , ''Param
--     , ''Instance
--     , ''Network
--     , ''NetworkDecl
--     , ''Bus
--     , ''BusSignal
--     , ''Range
--     , ''Process
--     , ''Generate
--     , ''Declaration
--     , ''Variable
--     , ''Constant
--     , ''Function
--     , ''Statement
--     , ''Enumeration
--     , ''Direction
--     , ''Expr
--     , ''BinOp
--     , ''UnOp
--     , ''Name
--     , ''ArrayIndex
--     , ''Type
--     , ''Literal
--     , ''SrcSpan
--     , ''SourcePos
--     , ''Pos
--     ]
