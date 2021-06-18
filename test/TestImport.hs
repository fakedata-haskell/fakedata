module TestImport where

import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import qualified Faker as F
import System.IO.Unsafe
import Test.Hspec (Expectation, shouldSatisfy)

defaultFakerSettings :: F.FakerSettings
defaultFakerSettings =
  unsafePerformIO $ do
    cfile <- newIORef HM.empty
    cfield <- newIORef HM.empty
    let settings = F.defaultFakerSettings
    s1 <- F.replaceCacheField HM.empty settings
    s2 <- F.replaceCacheFile HM.empty s1
    return s2

shouldBeOneOf :: (Eq a, Show a) => a -> [a] -> Expectation
shouldBeOneOf x xs = x `shouldSatisfy` (`elem` xs)
