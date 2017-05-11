module Data.ByteString where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString.Internal as B
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)

import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)


-- | The empty 'ByteString'
empty :: B.ByteString
empty = B.ByteString B.emptyBuf 0 0

-- | A byte string with a single byte.
-- |
-- | Running time: `O(1)`
singleton :: B.Octet -> B.ByteString
singleton s = B.unsafeCreate 1 \b -> Fn.runFn3 B.setAtOffset 0 s b

-- | Convert an array of bytes to byte string
-- |
-- | Running time: `O(n)`
pack :: Array B.Octet -> B.ByteString
pack = B.unsafeFromArray

-- | Converts a 'ByteString' to an array of bytes
-- |
-- | Running time: `O(n)`
unpack :: B.ByteString -> Array B.Octet
unpack = unsafePerformEff <<< (Buffer.toArray <=< B.toBuffer)

-- | Test whether a ByteString is empty.
-- |
-- | Running time: `O(1)`
null :: B.ByteString -> Boolean
null (B.ByteString _ _ i) = i <= 0

-- | 'length' returns the length of a ByteString as an 'Int'
-- |
-- | Running time: `O(1)`
length :: B.ByteString -> Int
length (B.ByteString _ _ le) = le

-- | 'cons' is analogous to (:) for lists, but have different complexity
-- |
-- | Running time: `O(n)`
cons :: B.Octet -> B.ByteString -> B.ByteString
cons c (B.ByteString buf ofs len) = B.unsafeCreate (len + 1) \buf' -> do
  _ <- Buffer.write Buffer.UInt8 c 0 buf'
  _ <- Fn.runFn5 B.copyBuffer ofs len buf 1 buf'
  pure unit

-- | Append a byte to the end of a 'ByteString'
-- |
-- | Running time: `O(n)`
snoc :: B.ByteString -> B.Octet -> B.ByteString
snoc (B.ByteString buf ofs len) c = B.unsafeCreate (len + 1) \buf' -> do
  _ <- Fn.runFn5 B.copyBuffer ofs len buf 0 buf'
  _ <- Buffer.write Buffer.UInt8 c 0 buf'
  pure unit

-- | Extract the first element of a ByteString, if the ByteString is empty then
-- | it return Nothing, `Just Octed`
-- |
-- | Running time: `O(1)`
head :: B.ByteString -> Maybe B.Octet
head (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ unsafePerformEff $ Buffer.read Buffer.UInt8 s x

-- | Extract the elements after the head of a ByteString
-- |
-- | Running time: `O(n)`
tail :: B.ByteString -> Maybe B.ByteString
tail (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ B.ByteString x (s + 1) (l - 1)

index :: B.ByteString -> Int -> Maybe B.Octet
index (B.ByteString x s l) n
  | n < 0      = Nothing
  | n >= l     = Nothing
  | otherwise  = unsafePerformEff $ Buffer.getAtOffset (n + s) x

-- | Extract the head and tail of a ByteString, returning Nothing if it is empty.
-- |
-- | Running time: `O(n)`
uncons :: B.ByteString -> Maybe { head :: B.Octet, tail :: B.ByteString }
uncons (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff do
      h <- Buffer.read Buffer.UInt8 s x
      pure $ Just $ { head: h, tail: B.ByteString x (s + 1) (l - 1) }

-- | Extract the last element of a ByteString
last :: B.ByteString -> Maybe B.Octet
last (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff $ Buffer.getAtOffset (l - 1) x

-- | Return all the elements of a 'ByteString' except the last one.
init :: B.ByteString -> Maybe B.ByteString
init bs@(B.ByteString x s l)
  | null bs   = Nothing
  | otherwise = Just $ B.ByteString x s (l - 1)

unsnoc :: B.ByteString -> Maybe { init :: B.ByteString, last :: B.Octet }
unsnoc bs@(B.ByteString x s l)
  | null bs    = Nothing
  | otherwise  = unsafePerformEff do
      lst <- Buffer.read Buffer.UInt8 (l - 1) x
      pure $ Just $ { init: B.ByteString x s (l - 1), last: lst }

reverse :: B.ByteString -> B.ByteString
reverse (B.ByteString buf ofs len) = B.unsafeCreate len \buf' ->
  Fn.runFn3 B.bufferReverse buf buf' len

intersperse :: B.Octet -> B.ByteString -> B.ByteString
intersperse c ps@(B.ByteString buf ofs len)
  | len < 2   = ps
  | otherwise = B.unsafeCreate (2 * len - 1) $ \buf' ->
      Fn.runFn6 B.intersperse ofs buf 0 buf' (len - ofs) c

foldl :: forall a. (a -> B.Octet -> a) -> a -> B.ByteString -> a
foldl _ a (B.ByteString _ _ 0)       = a
foldl f i (B.ByteString buf ofs len) = Fn.runFn5 B.foldl f i ofs len buf

foldr :: forall a. (B.Octet -> a -> a) -> a -> B.ByteString -> a
foldr _ a (B.ByteString _ _ 0)       = a
foldr f i (B.ByteString buf ofs len) = Fn.runFn5 B.foldr f i ofs len buf

foldMap :: forall m. Monoid m => (B.Octet -> m) -> B.ByteString -> m
foldMap f = foldr (\x acc -> f x <> acc) mempty

all :: forall b. HeytingAlgebra b => (B.Octet -> b) -> B.ByteString -> b
all = alaF Conj foldMap

any :: forall b. HeytingAlgebra b => (B.Octet -> b) -> B.ByteString -> b
any = alaF Disj foldMap

-- -----------------------------------------------------------------------------
-- Substrings ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns the first `n` bytes of the ByteString.
take :: Int -> B.ByteString -> B.ByteString
take n bs@(B.ByteString x s l)
  | n <= 0    = empty
  | n >= l    = bs
  | otherwise = B.ByteString x s n

-- | Return the ByteString without the first `n` bytes
drop :: Int -> B.ByteString -> B.ByteString
drop n bs@(B.ByteString x s l)
  | n <= 0    = bs
  | n >= l    = empty
  | otherwise = B.ByteString x (s + n) (l - n)

splitAt :: Int -> B.ByteString -> { before :: B.ByteString, after :: B.ByteString }
splitAt n bs@(B.ByteString x s l)
  | n <= 0    = { before: empty, after: bs }
  | n >= l    = { before: bs, after: empty }
  | otherwise = { before: B.ByteString x s n, after: B.ByteString x ( s + n) (l - n) }

--------------------------------------------------------------------------------
-- ByteString convertion -------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create ByteString from a string
fromString :: String -> Encoding -> B.ByteString
fromString s = unsafePerformEff <<< B.fromString s

-- | Convert ByteString to string
toString :: B.ByteString -> Encoding -> String
toString bs enc = unsafePerformEff $ Buffer.toString enc =<< B.toBuffer bs

-- | Create ByteString from Buffer, this consume entire length of Buffer even if
-- | the buffer still not written
fromBuffer :: Buffer -> B.ByteString
fromBuffer buf = B.ByteString buf 0 (unsafePerformEff $ Buffer.size buf)

-- | Convert ByteString to Buffer
toBuffer :: B.ByteString -> Buffer
toBuffer = B.unsafeToBuffer
