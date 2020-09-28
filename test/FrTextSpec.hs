{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FrTextSpec where

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

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "fr"

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
    it "validates fr-CH locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.prefix
            , NA.name
            , NA.nameWithMiddle
              
            , BO.title
            , BO.author
            , BO.publisher

            , PO.names
            , PO.locations
            , PO.moves

            , PH.formats
            , PH.cellPhoneFormat
            , PH.countryCode

            , FA.city
            , FA.state
            , FA.countryCode
            , FA.buildingNumber
            , FA.secondaryAddress
            , FA.streetSuffix
            , FA.streetName
            , FA.streetAddress
            , FA.fullAddress
            , FA.postcode
              
            , CL.name
            , CE.cardinalWord
            , CE.cardinalAbbreviation
            , CE.ordinalWord
            , CE.ordinalAbbreviation
            , CE.halfWindWord
            , CE.halfWindAbbreviation
            , CE.direction
            , CE.abbreviation

            , IN.freeEmail
            , IN.domainSuffix

            , CO.suffix
            , CO.name
            , CO.buzzword
            , CO.bs

            , LO.words
            , LO.supplemental
            , ME.metricHeight
            , ME.metricLength
            , ME.metricVolume
            , ME.metricWeight
            , AP.equipment
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
