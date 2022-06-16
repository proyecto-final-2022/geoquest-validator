module TestUtils where

import Data.Time (UTCTime, addUTCTime, nominalDay)
import Test.QuickCheck (elements, Gen, vectorOf)
import Data.Text (Text, pack)


addDaysToUTCTime :: Int -> UTCTime -> UTCTime
addDaysToUTCTime days = addUTCTime (nominalDay * fromIntegral days)


textHashGen :: Gen Text
textHashGen = pack <$> vectorOf 64 (elements hexDigits)
    where
        hexDigits = "0123456789abcdef"
