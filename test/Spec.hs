import Test.Hspec

import qualified ValidatorSpec as Validator


main :: IO ()
main = hspec $ do
    Validator.spec
