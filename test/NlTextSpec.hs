{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module NlTextSpec where

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
import qualified Faker.University as UN

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "nl"

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
    it "validates nl locale" $ do
      let functions :: [Fake Text] =
            [ 
            --   NA.lastName
            -- , NA.firstName
            -- , NA.prefix
            -- , NA.suffix
            -- , NA.name
            -- , NA.nameWithMiddle

              PH.formats
              
            , UN.prefix
            , UN.name

            , FA.citySuffix
            , FA.cityPrefix
            , FA.city
            , FA.country
            -- , FA.postcode
            , FA.buildingNumber
            , FA.secondaryAddress
            , FA.state
            , FA.streetSuffix
            , FA.streetName
            , FA.streetAddress
              
            , CO.suffix
              
            , IN.domainSuffix
            , IN.freeEmail
            , LO.words
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
