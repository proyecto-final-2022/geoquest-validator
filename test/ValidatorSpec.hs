module ValidatorSpec
    ( spec
    ) where


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Database.Persist.Sql (toSqlKey)
import Data.Char (isHexDigit)
import Data.Time (getCurrentTime)


import Types
import Validator (hashCoupon, isValid)
import DB (Coupon (..), ClientId, CouponId)
import ArbitraryInstances
import TestUtils
import Utils (hashFromText)


spec :: Spec
spec = describe "Validator API" $ do
    hashCouponSpec
    isValidSpec


hashCouponSpec :: Spec
hashCouponSpec = describe "hashCoupon" $ do
    prop "always returns a 64 hexadecimal characters long digest [SHA256: 256 bits]" $
        \id coupon -> length (show . couponHashDigest $ hashCoupon id coupon) `shouldBe` 64

    prop "always returns an all hexadecimal characters digest" $
        \id coupon -> (show . couponHashDigest $ hashCoupon id coupon) `shouldSatisfy` all isHexDigit


isValidSpec :: Spec
isValidSpec = describe "isValid" $ do
    it "is true if all conditions are satisfied" $ do
        currentTime <- getCurrentTime
        let cId = toSqlKey 1
        coupon <- testCoupon False False
        let hash = hashCoupon cId coupon
        isValid cId hash coupon `shouldReturn` True

    it "is not true if the given hash differs from the coupon's calculated hash" $ do
        currentTime <- getCurrentTime
        let cId = toSqlKey 1
        coupon <- testCoupon False False
        let (Right hash) = hashFromText "0B894166D3336435C800BEA36FF21B29EAA801A52F584C006C49289A0DCF6E2F"
        isValid cId hash coupon `shouldReturn` False


    it "is not true if the coupon has already expired" $ do
        currentTime <- getCurrentTime
        let cId = toSqlKey 1
        coupon <- testCoupon True False
        let hash = hashCoupon cId coupon
        isValid cId hash coupon `shouldReturn` False

    it "is not true if the coupon has already been used" $ do
        currentTime <- getCurrentTime
        let cId = toSqlKey 1
        coupon <- testCoupon False True
        let hash = hashCoupon cId coupon
        isValid cId hash coupon `shouldReturn` False

    where
        testCoupon expired used = do
            currentTime <- getCurrentTime
            let daysToAdd = if expired then (-1) else 1
            pure Coupon { couponClientId = toSqlKey 10
                        , couponUserId = 10
                        , couponDescription = "description"
                        , couponExpirationDate = addDaysToUTCTime daysToAdd currentTime
                        , couponUsed = used
                        }
   
