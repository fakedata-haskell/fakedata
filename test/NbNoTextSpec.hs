{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module NbNoTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Book as BO
import qualified Faker.Lorem as LO
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Appliance as AP
import qualified Faker.Measurement as ME
import qualified Faker.Compass as CE
import qualified Faker.Color as CL
import qualified Faker.Subscription as SU

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "nb-NO"

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
    it "validates nb-NO locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.femaleFirstName
            , NA.maleFirstName
            , NA.firstName
            , NA.prefix
            , NA.suffix
            , NA.name
            , NA.nameWithMiddle

            , PH.formats
            , PH.cellPhoneFormat

            -- , FA.citySuffix
            -- , FA.city
            -- , FA.postcode
            -- , FA.buildingNumber
            -- , FA.secondaryAddress
            -- , FA.state
            -- , FA.streetSuffix
            -- , FA.streetName
            -- , FA.streetAddress
              
            , CO.suffix
            , CO.name
              
            , IN.domainSuffix
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
