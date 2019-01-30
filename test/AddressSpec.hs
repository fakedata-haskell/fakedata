{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AddressSpec where

import Config
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker
import Faker.Address
import Test.Hspec

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
    it "resolve field" $ do
      let field = resolveFields "#{community_prefix} #{community_suffix}"
      field `shouldBe` ["community_prefix", "community_suffix"]
    it "resolve field with dot" $ do
      let field =
            resolveFields "#{city_prefix} #{Name.first_name}#{city_suffix}"
      field `shouldBe` ["city_prefix", "Name.first_name", "city_suffix"]
    it "resolve field with commas" $ do
      let field =
            resolveFields
              "#{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}"
      field `shouldBe`
        [ "secondary_address"
        , "street_address"
        , "city"
        , "state_abbr"
        , "zip_code"
        ]
    it "resolveCommunityField" $ do
      item <- resolveCommunityField defaultFakerSettings "community_suffix"
      item `shouldSatisfy` (\x -> T.length x > 0)
    it "uncons2" $ do
      let item = uncons2 "hello"
      item `shouldBe` Just ("he" :: String, "llo" :: Text)
    describe "operateField" $ do
      it "sample example" $ do
        let item = operateField "#{hello} #{world}" "jam"
        item `shouldBe` "jam #{world}"
      it "leading space example" $ do
        let item = operateField " #{hello} #{world}" "jam"
        item `shouldBe` " jam #{world}"
      it "trailing chars" $ do
        let item = operateField " #{hello} #{world} kool" "jam"
        item `shouldBe` " jam #{world} kool"
      it "edge case" $ do
        let item = operateField "this is" "jam"
        item `shouldBe` "this is"
