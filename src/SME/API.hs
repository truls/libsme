{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SME.API
  ( ValuePtr
  , SmeCtxPtr
  , BusPtr
  , ChannelVals
  , readPtr
  , writePtr
  , peek
  , poke
  , mkExtBus
  , mkExtChan
  ) where

import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.StablePtr     (StablePtr)
import           Foreign.Storable      (peek, poke)

import           SME.CTypes

import           Language.SMEIL.Syntax (Type (..))
import qualified SME.Representation    as R

data Bus
type BusPtr = Ptr Bus

data Chan
type ChanPtr = Ptr ChannelVals

type ChanValPtr = ForeignPtr ChannelVals

type ValuePtr = Ptr R.Value

data SmeCtx
newtype SimCtx = SimCtx SmeCtxPtr

type SmeCtxPtr = Ptr SmeCtx
type SimCtxPtr = StablePtr SimCtx


foreign export ccall "hs_sme_init"
  hsSmeInit :: SmeCtxPtr -> IO SimCtxPtr
foreign export ccall "clock_tick"
  clockTick :: SimCtxPtr -> IO SimCtxPtr

foreign import ccall "src/runner.h sme_add_bus"
               sme_add_bus :: SmeCtxPtr -> CString -> IO BusPtr
foreign import ccall unsafe "src/runner.h sme_get_read_val"
               sme_get_read_val :: SmeCtxPtr -> CString -> CString -> IO ValuePtr
foreign import ccall unsafe "src/runner.h sme_get_write_val"
               sme_get_write_val :: SmeCtxPtr -> CString -> CString -> IO ValuePtr
foreign import ccall "src/runner.h sme_add_chan" sme_add_chan ::
               BusPtr -> CString -> CInt -> IO ChanPtr
foreign import ccall "&free_chan_vals" free_chan_vals ::
               FunPtr (ChanPtr -> IO ())

mapToCType :: Type -> SMECType
mapToCType Signed {}   = SmeInt
mapToCType Unsigned {} = SmeUint
mapToCType Single {}   = SmeFloat
mapToCType Double {}   = SmeDouble
mapToCType Bool {}     = SmeBool
mapToCType _           = undefined

mkExtBus :: SmeCtxPtr -> String -> IO BusPtr
mkExtBus c s = withCString s $ sme_add_bus c

mkExtChan :: BusPtr -> String -> Type -> IO ChannelVals
mkExtChan bp s ct = do
  chanPtr <-
    withCString
      s
      (\x -> sme_add_chan bp x (fromIntegral $ fromEnum $ mapToCType ct))
  fptr <- newForeignPtr free_chan_vals chanPtr
  withForeignPtr fptr peek


clockTick :: SimCtxPtr -> IO SimCtxPtr
clockTick _ptr = undefined
  -- do
  -- (SimCtx ctx) <- deRefStablePtr ptr
  -- freeStablePtr ptr
  -- syncProcess ctx 0
  -- newStablePtr (SimCtx ctx)

hsSmeInit :: SmeCtxPtr -> IO SimCtxPtr
hsSmeInit _ptr = undefined
-- do
--   setupChans ptr
--   newStablePtr (SimCtx ptr)
