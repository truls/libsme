-- | Asynchronous CSV writer module
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}

module SME.CsvWriter
  ( writeCsvLine
  , CsvChan
  , mkCsvWriter
  , finalizeCsv
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.STM  (TMVar, TQueue, atomically,
                                          newEmptyTMVarIO, newTQueueIO,
                                          putTMVar, readTQueue, takeTMVar,
                                          writeTQueue)
import           Data.ByteString.Builder (Builder, charUtf8, doubleDec,
                                          floatDec, hPutBuilder, integerDec,
                                          stringUtf8)
import           Data.List               (intersperse)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8Builder)
import           System.IO               (IOMode (WriteMode), hClose, openFile)

import           SME.Representation

data CsvChan =
  CsvChan (TMVar ())
          (TQueue CsvCommand)

data CsvCommand where
  Write :: (ToCsvCell a) => [a] -> CsvCommand
  Shutdown :: CsvCommand

class ToCsvCell a where
  toCsvCell :: a -> Builder

instance ToCsvCell Value where
  toCsvCell (IntVal v)     = toCsvCell v
  toCsvCell (ArrayVal _ _) = undefined
  toCsvCell (BoolVal v)    = toCsvCell v
  toCsvCell (DoubleVal v)  = toCsvCell v
  toCsvCell (SingleVal v)  = toCsvCell v
  toCsvCell UndefVal       = toCsvCell "U"

instance ToCsvCell Integer where
  toCsvCell = integerDec

instance ToCsvCell Bool where
  toCsvCell True  = toCsvCell "true"
  toCsvCell False = toCsvCell "false"

instance ToCsvCell Double where
  toCsvCell = doubleDec

instance ToCsvCell Float where
  toCsvCell = floatDec

instance ToCsvCell [Char] where
  toCsvCell = stringUtf8

instance ToCsvCell Text where
  toCsvCell = encodeUtf8Builder

-- writeCsvLine :: (ToCsvCell a) => CsvChan -> [a] -> CsvChan
-- writeCsvLine chan its =            let b' =
--                  ((b <> (mconcat . intersperse (charUtf8 ',') . map toCsvCell) cs) <>
--                   charUtf8 '\n')
-- finalizeCsv :: CsvChan -> IO ()
-- finalizeCsv c = atomically $ writeTQueue c Shutdown
-- mkCsvWriter :: FilePath -> IO CsvChan
-- mkCsvWriter fileName = do
--   f <- openFile fileName WriteMode
finalizeCsv :: CsvChan -> IO ()
finalizeCsv (CsvChan s c) = do
  atomically $ writeTQueue c Shutdown
  atomically $ takeTMVar s

writeCsvLine :: (ToCsvCell a) => CsvChan -> [a] -> IO ()
writeCsvLine (CsvChan _ c) its = atomically $ writeTQueue c (Write its)

{-# INLINE writeCsvLine #-}
mkCsvWriter :: FilePath -> IO CsvChan
mkCsvWriter fileName = do
  f <- openFile fileName WriteMode
  chan <- newTQueueIO
  sync <- newEmptyTMVarIO
  _ <- forkIO $ loop mempty f chan sync
  return $ CsvChan sync chan
  where
    loop b f c s =
      atomically (readTQueue c) >>= \case
        Write cs ->
          let !b' =
                ((b <> (mconcat . intersperse (charUtf8 ',') . map toCsvCell) cs) <>
                 charUtf8 '\n')
          in loop b' f c s
        Shutdown -> do
          hPutBuilder f b
          hClose f
          atomically $ putTMVar s ()
