{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module ArbitraryInstances where

import Data.Time
import Database.Persist.Sql (toSqlKey, PersistEntity (EntityField, Key), SqlBackend, ToBackendKey)
import Data.Int (Int64)
import Test.QuickCheck
import Data.Text

import DB (Coupon (..), ClientId, CouponId)


type AnyId = Key

instance (ToBackendKey SqlBackend a) => Arbitrary (AnyId a) where
    arbitrary = toSqlKey <$> (arbitrary :: Gen Int64)

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary 
                        <*> (fromRational . toRational <$> choose (0::Double, 86400))

instance Arbitrary Coupon where
    arbitrary = do
        clientId <- arbitrary    
        expirationDate <- arbitrary
        description <- arbitrary
        used <- arbitrary
        pure $ Coupon { couponClientId = clientId
                      , couponExpirationDate = expirationDate
                      , couponDescription = description
                      , couponUsed = used
                      }



