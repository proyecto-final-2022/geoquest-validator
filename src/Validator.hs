module Validator
    ( checkCoupon
    , useCoupon
    , hashCoupon
    , isValid
    ) where


import Types
import DB
import Utils (runSQL, respondWithError, withDBAction)

import Crypto.Hash
import Web.Spock (json)
import qualified Network.HTTP.Types.Status as Status
import Data.Aeson (object, (.=))
import Data.ByteString.UTF8 (fromString)
import Database.Persist (get, update, (=.))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(liftIO))


{- HANDLERS -}

checkCoupon :: CouponId -> CouponHash -> AppActionM ()
checkCoupon id hash = do
    withDBAction "Coupon" (getCouponById id) $ \coupon -> do
        validity <- liftIO $ isValid id hash coupon
        json $ object [ "isValid" .= validity ]  

useCoupon :: CouponId -> CouponHash -> AppActionM ()
useCoupon id hash = do
    withDBAction "Coupon" (getCouponById id) $ \coupon -> do
        validity <- liftIO $ isValid id hash coupon
        if validity
        then notifyCouponUsage id
        else respondWithError Status.methodNotAllowed405 "Invalid coupon."


{- UTILS -}

-- ACTIONS

getCouponById :: CouponId -> AppActionM (Maybe Coupon)
getCouponById = runSQL . get

notifyCouponUsage :: CouponId -> AppActionM ()
notifyCouponUsage id = runSQL . update id $ 
    [ CouponUsed =. True ]

expired :: Coupon -> IO Bool
expired Coupon{..} = do
    time <- getCurrentTime
    pure $ couponExpirationDate < time

isValid :: CouponId -> CouponHash -> Coupon -> IO Bool
isValid id hash coupon = do
    hasExpired <- const <$> expired coupon
    pure $ all ($ coupon)
        [ matchesPrecalculatedHash id hash
        , not . couponUsed
        , not . hasExpired
        ]
    

-- PURE FUNCTIONS 

hashCoupon :: CouponId -> Coupon -> CouponHash
hashCoupon id Coupon{..} = CouponHash $ hash couponTextLine
    where couponTextLine = fromString $ show id
                                     ++ show couponClientId
                                     ++ show couponDescription
                                     ++ show couponUsed

matchesPrecalculatedHash :: CouponId -> CouponHash -> Coupon -> Bool
matchesPrecalculatedHash id hash coupon = hashCoupon id coupon == hash



