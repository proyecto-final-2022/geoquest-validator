module App
    ( app
    , getConfig
    ) where

import Web.Spock
import Web.Spock.Config
import qualified Network.HTTP.Types.Status as Status
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite hiding (get)
import Database.Persist.MySQL (createMySQLPool, ConnectInfo (..), defaultConnectInfo)
import Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)
import System.Environment (getEnv)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)

import Types
import DB
import Validator
import Utils
import Control.Monad (void)


loadEnv :: IO ()
loadEnv = onMissingFile (void . loadFile $ defaultConfig) (pure ())

getMySQLConnInfo :: IO ConnectInfo
getMySQLConnInfo = do
    loadEnv
    parsedConnInfo <- decode . fromString <$> getEnv "DB_CONNECTION" :: IO (Maybe ConnectInfo)
    case parsedConnInfo of
        Just dbConnInfo -> pure dbConnInfo
        Nothing         -> error "DB_CONNECTION: Missing or invalid ConnectInfo object environment variable."

getConfig :: IO (SpockCfg SqlBackend Session ())
getConfig = do
    dbConnInfo <- getMySQLConnInfo
    pool <- runStdoutLoggingT $ createMySQLPool dbConnInfo 5
    -- Un-comment to run DB migration.
    -- runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    runStdoutLoggingT $ runSqlPool (pure ()) pool
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

