module Types
    ( Session (..)
    , AppM
    , AppActionM
    , CouponHash (..)
    , ErrorMessage (..)
    ) where


import Web.Spock
import Data.Text
import Data.Aeson
import Database.Persist.Sql (SqlBackend)
import Crypto.Hash


data Session = EmptySession
type AppM a = SpockM SqlBackend Session () a
type AppActionM a = ActionCtxT () (WebStateM SqlBackend Session ()) a

newtype CouponHash = CouponHash { couponHashDigest :: Digest SHA256 }
    deriving (Show, Eq)

newtype ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage msg) = object [ "message" .= msg ]
            

