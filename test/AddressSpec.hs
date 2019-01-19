module AddressSpec where

import Faker
import Faker.Address
import Test.Hspec
import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "Address" $ do
         it "parses countries" $ do
                         ctries <- countries defaultFakerSettings
                         ctries `shouldSatisfy` (\x -> V.length x >= 40)
