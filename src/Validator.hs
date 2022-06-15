module Validator
    ( checkCoupon
    , useCoupon
    ) where


import Types
import DB

import Crypto.Hash
import Web.Spock (json, setStatus)
import qualified Network.HTTP.Types.Status as Status
import Utils (runSQLQuery, runSQL, respondWithError, withDBAction)
import Data.Aeson (object, (.=))
import Data.ByteString.UTF8 (fromString)
import Database.Persist (Entity, get, update, (=.))
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)
import Data.Int (Int64)


{- HANDLERS -}

checkCoupon :: CouponId -> CouponHash -> AppActionM ()
checkCoupon id hash = do
    withDBAction "Coupon" (getCouponById id) $ \coupon -> do
        validity <- isValid id hash coupon
        json $ object [ "isValid" .= validity ]  

useCoupon :: CouponId -> CouponHash -> AppActionM ()
useCoupon id hash = do
    withDBAction "Coupon" (getCouponById id) $ \coupon -> do
        validity <- isValid id hash coupon
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

expired :: Coupon -> AppActionM Bool
expired Coupon{..} = do
    time <- liftIO getCurrentTime
    pure $ couponExpirationDate < time

isValid :: CouponId -> CouponHash -> Coupon -> AppActionM Bool
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



