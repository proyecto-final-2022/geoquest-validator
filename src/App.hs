module App 
    ( app
    , getConfig
    ) where

import Web.Spock
import Web.Spock.Config
import Data.Text
import qualified Network.HTTP.Types.Status as Status
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite hiding (get)
import Data.Aeson (Value (String), eitherDecodeStrict)
import Data.Bifunctor (Bifunctor(first))
import Data.Text.Encoding (encodeUtf8)

import Types
import DB
import Validator
import Utils


getConfig :: IO (SpockCfg SqlBackend Session ())
getConfig = do
    pool <- runStdoutLoggingT $ createSqlitePool "./db/test.db" 5
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    defaultSpockCfg EmptySession (PCPool pool) ()


app :: AppM ()
app = do 
    get ("coupons" <//> var <//> var) $ \id rawHash -> do
        case hashFromText rawHash of
            Left errorMsg -> respondWithError Status.badRequest400 errorMsg
            Right hash    -> checkCoupon id hash
    
    patch ("coupons" <//> var <//> var) $ \id rawHash -> do
        case hashFromText rawHash of
            Left errorMsg -> respondWithError Status.badRequest400 errorMsg
            Right hash    -> useCoupon id hash



