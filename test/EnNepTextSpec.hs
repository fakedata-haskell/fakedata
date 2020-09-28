{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnNepTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Bank as BH

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "en-NEP"

fakerSettings :: FakerSettings
fakerSettings = setLocale locale defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates en-NEP locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.maleFirstName
            , NA.femaleFirstName
            , NA.name
            , NA.firstName
            , NA.lastName
            , NA.nameWithMiddle

            , FA.city
            , FA.postcode
            , FA.state

            , IN.freeEmail
            , IN.domainSuffix

            , PH.formats
            , PH.countryCode
              
            , CO.suffix
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
