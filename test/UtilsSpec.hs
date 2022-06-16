module UtilsSpec where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)
import Data.Text (pack, toLower)
import Data.Either (isLeft, isRight)

import TestUtils
import Utils (hashFromText)
import Types (CouponHash(couponHashDigest))


spec :: Spec
spec = describe "Utils" $ do
    hashFromTextSpec


hashFromTextSpec :: Spec
hashFromTextSpec = describe "hashFromText" $ do
    prop "always returns the original text value when reversed" $ do
        forAll textHashGen $ \t -> (pack . show . couponHashDigest <$> hashFromText t) `shouldBe` Right t 

    it "returns Left if the given text is shorter than 64 characters" $ do
        let t = "adbf12392123"
        hashFromText t `shouldSatisfy` isLeft

    it "returns Left if the given text contains a non hexadecimal character" $ do
        let t = "0B894166D3336435C800BEA36FF21B29EAA801A52F584C006C49289A0DCF6E2" <> "Q"  -- Append non hex char 'Q'
        hashFromText t `shouldSatisfy` isLeft

    it "is not case sensitive" $ do
        let t = "0B894166D3336435C800BEA36FF21B29E" <> toLower "AA801A52F584C006C49289A0DCF6E2E" 
        hashFromText t `shouldSatisfy` isRight




