module Data.ByteString.Internal
  ( ByteString(..), Octet, Offset, Size, EffBuf
  , unsafeCreate, unsafeFromArray, unsafeToBuffer, unsafeBufferSlice
  , create, createUptoN, createAndTrim, fromArray, fromString, toBuffer
  , setAtOffset, bufferSlice, bufferCompare, copyBuffer, emptyBuf, bufferReverse
  , intersperse, foldl, foldr
  , _isSpace, _isSpaceChar
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.Function.Uncurried as Fn
import Data.Monoid (class Monoid)

import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)


-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | Type synonym indicating the value refers to a length of a buffer
type Size = Int

-- | An effect used by this module
type EffBuf e = Eff (buffer :: BUFFER | e)

-- | A 'ByteString' contains 8-bit bytes, it's implemented using Buffer.
data ByteString = ByteString Buffer Offset Size

instance eqByteString :: Eq ByteString where
  eq = eqBS

instance ordByteString :: Ord ByteString where
  compare = compareBS

instance semigroupByString :: Semigroup ByteString where
  append = appendBS

instance monoidByString :: Monoid ByteString where
  mempty = ByteString emptyBuf 0 0

instance showByteString :: Show ByteString where
  show s = "(Buffer unpack " <> show (unsafePerformEff $ Buffer.toArray $ unsafeToBuffer s) <> " )"

eqBS :: ByteString -> ByteString -> Boolean
eqBS a@(ByteString bs off len) b@(ByteString bs' off' len')
  | len /= len'                = false
  | otherwise                  = EQ == compareBS a b

compareBS :: ByteString -> ByteString -> Ordering
compareBS (ByteString _ _ 0) (ByteString _ _ 0)               = EQ
compareBS (ByteString bs1 of1 len1) (ByteString bs2 of2 len2) =
  compare 0 $ unsafePerformEff do
    bs1' <- Fn.runFn3 bufferSlice of1 len1 bs1
    bs2' <- Fn.runFn3 bufferSlice of2 len2 bs2
    Fn.runFn2 bufferCompare bs1' bs2'

appendBS :: ByteString -> ByteString -> ByteString
appendBS (ByteString _ _ 0) b                  = b
appendBS a                  (ByteString _ _ 0) = a
appendBS (ByteString b1 of1 len1) (ByteString b2 of2 len2)
  | of1 + len1 == of2 && bufferEqual b1 b2 =
      ByteString b1 of1 (len1 + len2)
  | otherwise =
      unsafeCreate (len1+len2) \buf -> do
        _ <- Fn.runFn5 copyBuffer of1 len1 b1 0 buf
        _ <- Fn.runFn5 copyBuffer of2 len2 b2 (len1 - of1) buf
        pure unit

unsafeCreate :: forall e. Size -> (Buffer -> EffBuf e Unit) -> ByteString
unsafeCreate s f = unsafePerformEff $ create s f

unsafeFromArray :: Array Octet -> ByteString
unsafeFromArray = unsafePerformEff <<< fromArray

-- | convert a ByteString to Buffer
unsafeToBuffer :: ByteString -> Buffer
unsafeToBuffer = unsafePerformEff <<< toBuffer

unsafeBufferSlice :: Int -> Int -> Buffer -> Buffer
unsafeBufferSlice off len buf = unsafePerformEff $ Fn.runFn3 bufferSlice off len buf

create :: forall e. Size -> (Buffer -> EffBuf e Unit) -> EffBuf e ByteString
create size f = do
  buf <- Buffer.create size
  _ <- f buf
  pure $ ByteString buf 0 size

createUptoN :: forall e. Size -> (Buffer -> EffBuf e Int) -> EffBuf e ByteString
createUptoN size f = do
  buf <- Buffer.create size
  len <- f buf
  pure $ ByteString buf 0 (min size len)

createAndTrim :: forall e. Size -> (Buffer -> EffBuf e Int) -> EffBuf e ByteString
createAndTrim size f = do
  buf <- Buffer.create size
  len <- f buf
  if len >= size
    then pure $ ByteString buf 0 size
    else
      create len \buf' -> do
         _ <- Fn.runFn5 copyBuffer 0 len buf 0 buf'
         pure unit

toBuffer :: forall e. ByteString -> EffBuf e Buffer
toBuffer (ByteString src ofs len) = Fn.runFn3 bufferSlice ofs len src

fromArray :: forall e. Array Octet -> EffBuf e ByteString
fromArray xs = do
  buf <- Buffer.fromArray xs
  size <- Buffer.size buf
  pure $ ByteString buf 0 size

fromString ::forall e. String -> Encoding -> EffBuf e ByteString
fromString s enc = do
  buf <- Buffer.fromString s enc
  size <- Buffer.size buf
  pure $ ByteString buf 0 size

_isSpace :: Octet -> Boolean
_isSpace c =
  c == 0x20 ||
  c == 0x0A || -- LF, \n
  c == 0x09 || -- HT, \t
  c == 0x0C || -- FF, \f
  c == 0x0D || -- CR, \r
  c == 0x0B || -- VT, \v
  c == 0xA0    -- spotted by QC..

_isSpaceChar :: Char -> Boolean
_isSpaceChar c =
  c == ' '     ||
  c == '\t'    ||
  c == '\n'    ||
  c == '\r'    ||
  c == '\f'    ||
  c == '\v'    ||
  c == '\xa0'

bufferEqual :: Buffer -> Buffer -> Boolean
bufferEqual a b = unsafePerformEff $ eq 0 <$> Fn.runFn2 bufferCompare a b

foreign import foldl :: forall a. Fn.Fn5 (a -> Octet -> a) a Offset Size Buffer a

foreign import foldr :: forall a. Fn.Fn5 (Octet -> a -> a) a Offset Size Buffer a

foreign import bufferReverse :: forall e. Fn.Fn3 Buffer Buffer Size (EffBuf e Unit)

foreign import intersperse :: forall e. Fn.Fn6 Offset Buffer Offset Buffer Int Octet (EffBuf e Unit)

foreign import setAtOffset :: forall e. Fn.Fn3 Offset Octet Buffer (EffBuf e Unit)

foreign import bufferCompare :: forall e. Fn.Fn2 Buffer Buffer (EffBuf e Int)

foreign import bufferSlice :: forall e. Fn.Fn3 Int Int Buffer (EffBuf e Buffer)

foreign import copyBuffer :: forall e. Fn.Fn5 Offset Offset Buffer Offset Buffer (EffBuf e Int)

foreign import emptyBuf :: Buffer
