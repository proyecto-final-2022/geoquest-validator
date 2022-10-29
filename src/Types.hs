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
import Database.Persist.MySQL (ConnectInfo (..), defaultConnectInfo)


data Session = EmptySession
type AppM a = SpockM SqlBackend Session () a
type AppActionM a = ActionCtxT () (WebStateM SqlBackend Session ()) a

newtype CouponHash = CouponHash { couponHashDigest :: Digest SHA256 }
    deriving (Show, Eq)

newtype ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage msg) = object [ "message" .= msg ]
            
instance FromJSON ConnectInfo where
    parseJSON = withObject "ConnectInfo" $ \o -> do
        user     <- o .: "username"
        password <- o .: "password"
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .: "port"
        pure $ defaultConnectInfo { connectUser     = user
                                  , connectPassword = password
                                  , connectDatabase = database
                                  , connectHost     = host
                                  , connectPort     = port 
                                  }
