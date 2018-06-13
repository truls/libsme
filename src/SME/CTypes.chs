{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- For the R.Value instance. We want to run as little as possible through the
-- c2hs preprocessor
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SME.CTypes (SMECType(..), ChannelVals, readPtr, writePtr) where

import Foreign.C.Types (CChar, CFloat(..), CDouble(..))
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import GHC.Exts (Word(..), Ptr(..));

import GHC.Integer.GMP.Internals (importIntegerFromAddr,
                                  exportIntegerToAddr,
                                  sizeInBaseInteger)

import qualified SME.Representation as R

#include "libsme.h"

{# enum Type as SMECType {underscoreToCase} deriving (Eq, Show) #}

data SMEInteger = SMEInteger
  { negative :: Bool
  , len :: Word
  , addr :: Ptr CChar
  , _valPtr :: Ptr SMEInteger
  }

data Signedness = Signed | Unsigned
  deriving (Eq)

data ChannelVals = ChannelVals
  { readPtr :: Ptr R.Value
  , writePtr :: Ptr R.Value
  }

foreign import ccall "sme_integer_resize"
    sme_integer_resize :: Ptr SMEInteger -> Word -> IO()

instance Storable ChannelVals where
  sizeOf _ = {# sizeof ChannelVals #}
  alignment _ = {# alignof ChannelVals #}

  poke p value = do
    let rp = (castPtr :: Ptr R.Value -> Ptr ()) (readPtr value)
        wp = (castPtr :: Ptr R.Value -> Ptr ()) (writePtr value)
    {# set ChannelVals.read_ptr #} p rp
    {# set ChannelVals.write_ptr #} p wp
  {-# INLINE poke #-}

  peek p = do
    rp <- {# get ChannelVals.read_ptr #} p
    wp <- {# get ChannelVals.write_ptr #} p
    let cast = (castPtr :: Ptr () -> Ptr R.Value)
    return $ ChannelVals (cast rp) (cast wp)
  {-# INLINE peek #-}

instance Storable SMEInteger where
  sizeOf _ = {# sizeof SMEInt #}
  alignment _ = {# alignof SMEInt #}

  poke p value = do
    -- All other fields are non modifiable from here
    {# set SMEInt.negative #} p (if (negative value) then 1 else 0)
  {-# INLINE poke #-}

  peek p =
    SMEInteger <$> ((/= 0) <$> {# get SMEInt.negative #} p)
               <*> (fromIntegral <$> {# get SMEInt.len #} p)
               <*> {# get SMEInt.num #} p
               <*> pure p
  {-# INLINE peek #-}


pokeIntVal :: Signedness -> (t -> IO (Ptr ())) -> t -> Integer -> IO ()
pokeIntVal signedness f p val = do
  let bytes = if val == 0 then 1 else W# (sizeInBaseInteger val 256#)
  iptr' <- f p
  let iptr = (castPtr :: Ptr () -> Ptr SMEInteger) iptr'
  sme_integer_resize iptr bytes
  intRep <- peek iptr
  let !(Ptr unpackedAddr) = addr intRep
  _ <- exportIntegerToAddr val unpackedAddr 0#
  poke iptr (intRep { negative = ((signedness == Signed) && (val < 0)) })
{-# INLINE pokeIntVal #-}

peekIntVal :: Signedness -> (t -> IO (Ptr())) -> t -> IO Integer
peekIntVal signedness f p = do
  iptr <- f p
  intRep <- peek ((castPtr :: Ptr () -> Ptr SMEInteger) iptr)
  let !(W# unpackedLen) = len intRep
  let !(Ptr unpackedAddr) = addr intRep
  res <- importIntegerFromAddr unpackedAddr unpackedLen 0#
  return $ case signedness of
    Signed -> if negative intRep then negate res else res
    Unsigned -> res
{-# INLINE peekIntVal #-}

setValUndef :: Bool -> Ptr R.Value -> IO ()
setValUndef b f = {# set Value.undef #} f (fromIntegral $ fromEnum b)

instance Storable R.Value where
  sizeOf _ = {# sizeof Value #}
  alignment _ = {# alignof Value #}

  poke p value =
      case value of
        R.IntVal i -> do
          pokeIntVal Signed {# get Value.value.integer #} p i
          setValUndef False p
        R.SingleVal i -> do
          {# set Value.value.f32 #} p $ CFloat i
          setValUndef False p
        R.DoubleVal i -> do
          {# set Value.value.f64 #} p $ CDouble i
          setValUndef False p
        R.BoolVal i -> do
          {# set Value.value.boolean #} p $ i
          setValUndef False p
        R.ArrayVal _ _ -> error "Arrays not supported"
        R.UndefVal ->
          setValUndef True p
  {-# INLINE poke #-}

  peek p =
      (\x -> if x == 0 then False else True) <$> ({# get Value.undef #} p) >>= \case
        True -> pure R.UndefVal
        False ->
          ((toEnum . fromIntegral) <$> ({# get Value.type #} p)) >>= \case
           SmeInt ->
             R.IntVal <$> peekIntVal Signed {# get Value.value.integer #} p
           SmeUint ->
             R.IntVal <$> peekIntVal Unsigned {# get Value.value.integer #} p
           SmeFloat -> do
             (CFloat f) <- ({# get Value.value.f32 #} p)
             return $ R.SingleVal f
           SmeDouble -> do
             (CDouble f) <- ({# get Value.value.f64 #} p)
             return $ R.DoubleVal f
           SmeBool ->
             R.BoolVal <$> ({# get Value.value.boolean #} p)
  {-# INLINE peek #-}
