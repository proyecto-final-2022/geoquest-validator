module TestUtils where

import Data.Time (UTCTime, addUTCTime, nominalDay)

addDaysToUTCTime :: Int -> UTCTime -> UTCTime
addDaysToUTCTime days = addUTCTime (nominalDay * fromIntegral days)
