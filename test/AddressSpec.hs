{-#LANGUAGE ScopedTypeVariables#-}

module AddressSpec where

import Faker
import Faker.Address
import Test.Hspec
import qualified Data.Vector as V
import Config
import Data.Text (Text)

spec :: Spec
spec = do
  describe "Address" $ do
    it "parses countries for defaultSettings" $ do
      ctries <- countries defaultFakerSettings
      ctries `shouldSatisfy` (\x -> V.length x >= 40)
    it "parses countries for other locales" $ do
      locales <- populateLocales
      let settings :: [FakerSettings] =
            map (\l -> setLocale l defaultFakerSettings) locales
          ctries :: IO [V.Vector Text] = mapM countries settings
      ctries' :: [V.Vector Text] <- ctries
      let exp :: [Bool] = map (\x -> V.length x >= 10) ctries'
      True `shouldBe` (all (\x -> x == True) exp)
