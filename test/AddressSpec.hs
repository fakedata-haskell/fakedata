{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE OverloadedStrings#-}

module AddressSpec where

import Faker
import Faker.Address
import Test.Hspec
import qualified Data.Vector as V
import Config
import Data.Text hiding (map, all)
import qualified Data.Text as T

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
    it "produces random integer text" $ do
       txt <- interpolateNumbers "#####"
       let num :: Int = read (unpack txt)
       num `shouldSatisfy` (\x -> x >= 0 && T.length txt == 5)
    it "produces random integer only for hash" $ do
       txt <- interpolateNumbers "ab-##"
       txt `shouldSatisfy` (\x -> T.length x == 5 && (T.take 3 x == "ab-"))
