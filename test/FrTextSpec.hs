{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FrTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Creature.Animal as CA
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
import qualified Faker.Demographic as DE
import qualified Faker.Color as CL
import qualified Faker.Gender as GE

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
    it "validates fr locale" $ do
      let functions :: [Fake Text] =
            [

              NA.femaleFirstName
            , NA.maleFirstName
            , NA.lastName
            , NA.firstName
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
            , CE.cardinalAzimuth
            , CE.ordinalWord
            , CE.ordinalAbbreviation
            , CE.ordinalAzimuth
            , CE.halfWindWord
            , CE.halfWindAbbreviation
            , CE.halfWindAzimuth
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

            , CA.name

            , DE.sex

            , GE.types
            , GE.shortBinaryTypes
            , GE.binaryTypes
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
