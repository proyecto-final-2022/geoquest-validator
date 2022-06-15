{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}


module DB where

import Database.Persist
import Database.Persist.TH
import Data.Text
import Data.Time (UTCTime)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Coupon sql=coupons
        clientId       ClientId
        description    Text
        expirationDate UTCTime
        used           Bool
        deriving Show

    Client sql=clients
        name           Text
        deriving Show
|]


