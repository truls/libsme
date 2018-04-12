{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SME.API.Internal (
  ValuePtr
  , SmeCtxPtr
  , BusPtr
  , ChannelVals
  , mkExtBus
  , mkExtChan
  , readPtr
  , writePtr
  , peek
  , poke
) where


import           Foreign.C.String      (CString, withCString)
import           Foreign.C.Types       (CInt (..))
import           Foreign.ForeignPtr    (ForeignPtr, newForeignPtr,
                                        withForeignPtr)
import           Foreign.Ptr           (FunPtr, Ptr)
import           Foreign.Storable      (peek, poke)

import           Language.SMEIL.Syntax (Type (..))
import           SME.CTypes
import qualified SME.Representation    as R

data Chan
type ChanPtr = Ptr ChannelVals

data Bus
type BusPtr = Ptr Bus


type ValuePtr = Ptr R.Value

data SmeCtx

type SmeCtxPtr = Ptr SmeCtx

type ChanValPtr = ForeignPtr ChannelVals


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
