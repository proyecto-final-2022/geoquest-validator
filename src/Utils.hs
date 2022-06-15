{-# LANGUAGE GADTs  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Utils
    ( respondWithError
    , withDBAction
    , runSQL
    , runSQLQuery
    , hashFromText
    ) where


import Network.HTTP.Types.Status (Status)
import Types
import Data.Text
import Web.Spock (setStatus, json, runQuery, HasSpock (SpockConn))
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Maybe
import Database.Persist.Sql (runSqlConn, SqlBackend, SqlPersistT, SqlReadT, Entity, liftPersist, PersistStoreRead (get), toSqlKey, PersistEntity, PersistRecordBackend)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import qualified Network.HTTP.Types.Status as Status
import Data.Int (Int64)
import Crypto.Hash (digestFromByteString, Digest, SHA256)
import Data.Either.Extra (maybeToEither)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteArray.Encoding (convertFromBase, Base (Base16))
import Data.Bifunctor (first)
import Data.ByteString hiding (pack)


respondWithError :: Status -> Text -> AppActionM ()
respondWithError status message = do
    setStatus status
    json . ErrorMessage $ message


hashFromText :: Text -> Either Text CouponHash
hashFromText = (CouponHash <$>) . digestByteString . encodeUtf8
    where
        digestByteString :: ByteString -> Either Text (Digest SHA256)
        digestByteString str = first pack $ convertFromBase Base16 str >>= eitherDigestFromByteString

        eitherDigestFromByteString :: ByteString -> Either String (Digest SHA256)
        eitherDigestFromByteString = maybeToEither "Invalid hash type." . digestFromByteString


type SqlAction a = SqlPersistT (LoggingT IO) a

runSQL :: SqlAction a -> AppActionM a
runSQL action = runQuery $ \conn ->
    runStdoutLoggingT $ runSqlConn action conn


runSQLQuery :: (Functor f) => SqlReadT IO (f r) -> AppActionM (f r)
runSQLQuery = runSQL . liftPersist


withDBAction :: Text -> AppActionM (Maybe a) -> (a -> AppActionM ()) -> AppActionM ()
withDBAction resultName action doSomething =
    action >>= maybe
                (respondWithError Status.notFound404 (resultName <> " not found."))
                doSomething
               
