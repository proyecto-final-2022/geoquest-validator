import Test.Hspec

import qualified ValidatorSpec as Validator
import qualified UtilsSpec as Utils


main :: IO ()
main = hspec $ do
    Validator.spec
    Utils.spec
