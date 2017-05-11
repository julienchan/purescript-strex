module Test.Data.ByteString where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Test.Unit.Assert (shouldEqual)
import Test.Unit (TestSuite, describe, it)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(..))

import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString.Internal as BI
import Data.ByteString as B

newtype ByteStringTest = ByteStringTest BI.ByteString

derive instance newtypeByteStringTest :: Newtype ByteStringTest _
derive newtype instance eqByteStringTest :: Eq ByteStringTest
derive newtype instance ordByteStringTest :: Ord ByteStringTest

instance arbByteStringTest :: Arbitrary ByteStringTest where
  arbitrary = do
    xs <- (arbitrary :: Gen (Array OctetTest))
    pure $ ByteStringTest $ B.pack (unwrap <$> xs)

newtype OctetTest = OctetTest Int

derive instance newtypeOctetTest :: Newtype OctetTest _
derive newtype instance eqByteOctetTest :: Eq OctetTest
derive newtype instance ordByteOctetTest :: Ord OctetTest

instance arbOctetTest :: Arbitrary OctetTest where
  arbitrary = OctetTest <$> chooseInt 0 255

unOctetTest :: OctetTest -> Int
unOctetTest = unwrap

test_propertyLength :: ByteStringTest -> Boolean
test_propertyLength (ByteStringTest bts) =
  B.length bts == (unsafePerformEff (Buffer.size (BI.unsafeToBuffer bts)))

test_propertyIntersperse :: ByteStringTest -> Boolean
test_propertyIntersperse (ByteStringTest bts) =
  let sr = B.intersperse 0x2c bts
  in if B.length bts < 2
    then sr == bts
    else B.length sr == (2 * (B.length bts) - 1) && sr /= bts

test_propertyTakeByteString :: Int -> ByteStringTest -> Boolean
test_propertyTakeByteString n (ByteStringTest b)
  | n <= 0           = true
  | n >= B.length b  = b == B.take n b
  | otherwise        = B.length (B.take n b) == n

mainBS :: forall eff. TestSuite (buffer :: BUFFER, console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
mainBS = do
  describe "Data.ByteString" do
    it "semigroup instance" do
      let bs1 = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          bs2 = B.singleton 32
          bs3 = B.singleton 98
          concatenated = bs1 <> bs2 <> bs3
      B.toString concatenated UTF8  `shouldEqual` "tést b"
      (bs1 <> (bs2 <> bs3)) `shouldEqual` ((bs1 <> bs2) <> bs3)

    it "intersperse" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55]
          is = B.intersperse 0x2c bs
      sp <- liftEff $ Buffer.getAtOffset 1 (BI.unsafeToBuffer is)
      sp `shouldEqual` Just 0x2c

    it "concat, init, length" do
      let bs1  = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          bs2  = B.pack [0x98, 0x77]
          init' = B.init bs1
      case init' of
        Nothing -> pure unit
        Just init -> do
          len <- liftEff $ Buffer.size (BI.unsafeToBuffer bs1)
          len2 <- liftEff $ Buffer.size (BI.unsafeToBuffer init)
          (len2 + 1) `shouldEqual` len
          B.unpack init `shouldEqual` [0x74, 0xc3, 0xa9, 0x73]
          B.unpack (init <> bs2) `shouldEqual` [0x74, 0xc3, 0xa9, 0x73, 0x98, 0x77]

    it "property unpack, pack" do
      liftEff do
        quickCheck \b ->
          let c = unOctetTest <$> b
          in B.unpack (B.pack c) == c
        quickCheck \(ByteStringTest b) -> B.pack (B.unpack b) == b

    it "property index" do
      liftEff $ quickCheck test_propertyTakeByteString

    it "property reverse" do
      liftEff $ quickCheck \(ByteStringTest b) -> B.reverse (B.reverse b) == b

    it "property length" do
      liftEff $ quickCheck test_propertyLength

    it "property intersperse" do
      liftEff $ quickCheck test_propertyIntersperse

    it "property from string" do
      -- | todo seed all possible chars on UTF-8
      liftEff $ quickCheck \(AlphaNumString s) ->
        s == B.toString (B.fromString s UTF8) UTF8
