module Data.ByteString.Builder.Prim.Binary
  ( int8BE
  , int16BE
  , int32BE
  , int8LE
  , int16LE
  , int32LE
  , uint8BE
  , uint16BE
  , uint32BE
  , uint8LE
  , uint16LE
  , uint32LE
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)

import Data.ByteString.Internal (poke, plusPtr)
import Data.Int.Bits ((.&.), shr, zshr)
import Data.ByteString.Builder.Prim.Types (FixedPrim, fixedPrim)

-- | Encoding an integer of 8 bits in big-endian encoding.
int8BE :: FixedPrim Int
int8BE = fixedPrim 1 \i p -> liftEff $ poke p ((i `shr` 0) .&. 0xFF)

-- | Encoding a integer of 16 bits in big-endian encoding.
int16BE :: FixedPrim Int
int16BE = fixedPrim 2 \i p -> liftEff $
  poke p ((i `shr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `shr` 0) .&. 0xFF)

-- | Encoding a integer of 32 bits in big-endian encoding.
int32BE :: FixedPrim Int
int32BE = fixedPrim 4 \i p -> liftEff $
  poke p ((i `shr` 24) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `shr` 16) .&. 0xFF)
  *> poke (p `plusPtr` 2) ((i `shr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 3) ((i `shr` 0) .&. 0xFF)

int8LE :: FixedPrim Int
int8LE = int8BE

int16LE :: FixedPrim Int
int16LE = fixedPrim 2 \i p -> liftEff $
  poke p ((i `shr` 0) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `shr` 8) .&. 0xFF)

int32LE :: FixedPrim Int
int32LE = fixedPrim 4 \i p -> liftEff $
  poke p ((i `shr` 0) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `shr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 2) ((i `shr` 16) .&. 0xFF)
  *> poke (p `plusPtr` 3) ((i `shr` 24) .&. 0xFF)

uint8BE :: FixedPrim Int
uint8BE = fixedPrim 1 \i p -> liftEff $ poke p ((i `zshr` 0) .&. 0xFF)

uint16BE :: FixedPrim Int
uint16BE = fixedPrim 2 \i p -> liftEff $
  poke p ((i `zshr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `zshr` 0) .&. 0xFF)

uint32BE :: FixedPrim Int
uint32BE = fixedPrim 4 \i p -> liftEff $
  poke p ((i `zshr` 24) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `zshr` 16) .&. 0xFF)
  *> poke (p `plusPtr` 2) ((i `zshr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 3) ((i `zshr` 0) .&. 0xFF)

uint8LE :: FixedPrim Int
uint8LE = uint8BE

uint16LE :: FixedPrim Int
uint16LE = fixedPrim 2 \i p -> liftEff $
  poke p ((i `zshr` 0) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `zshr` 8) .&. 0xFF)

uint32LE :: FixedPrim Int
uint32LE = fixedPrim 4 \i p -> liftEff $
  poke p ((i `zshr` 0) .&. 0xFF)
  *> poke (p `plusPtr` 1) ((i `zshr` 8) .&. 0xFF)
  *> poke (p `plusPtr` 2) ((i `zshr` 16) .&. 0xFF)
  *> poke (p `plusPtr` 3) ((i `zshr` 24) .&. 0xFF)
