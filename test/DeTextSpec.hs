{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DeTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Company as CO
import qualified Faker.PhoneNumber as PH
import qualified Faker.Color as CL
import qualified Faker.Name as NA
import qualified Faker.Commerce as CE
import qualified Faker.Team as TE
import qualified Faker.App as AP
import qualified Faker.Internet as IN
import qualified Faker.TvShow.DrWho as DR
import qualified Faker.TvShow.Simpsons as SI
import qualified Faker.University as UN
import qualified Faker.Space as SP
import qualified Faker.Music as MU
import qualified Faker.Lorem as LO
import qualified Faker.Hipster as HI
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Food as FO
import qualified Faker.Book as BO
import qualified Faker.ChuckNorris as CN
import qualified Faker.Compass as CE

import Faker.Combinators (listOf)

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

fakerSettings :: FakerSettings
fakerSettings = setLocale "de" defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates de locale" $ do
      let functions :: [Fake Text] =
            [ 
             FA.cityPrefix
            , FA.citySuffix
            , FA.country
            , FA.countryCode
            , FA.buildingNumber
            , FA.secondaryAddress
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.timeZone
            , FA.city
            , FA.streetName
            , FA.streetAddress

            , CO.suffix
            , CO.name

            , CE.cardinalWord
            , CE.cardinalAbbreviation
            , CE.cardinalAzimuth
            , CE.ordinalWord
            , CE.ordinalAbbreviation
            , CE.ordinalAzimuth
            , CE.halfWindWord
            , CE.halfWindAbbreviation
            , CE.halfWindAzimuth
            , CE.quarterWindWord
            , CE.quarterWindAbbreviation
            , CE.quarterWindAzimuth
            , CE.direction
            , CE.azimuth
            , CE.abbreviation
              
            , IN.domainSuffix
            , IN.freeEmail

            , LO.words
            , HI.words
              
            , NA.firstName
            , NA.lastName
            , NA.prefix
            , NA.name
            , NA.nameWithMiddle
              
            , CL.name
              
            , CE.department
            , CE.productNameAdjective
            , CE.productNameMaterial
            , CE.productNameProduct
              
            , PH.formats
            , PH.cellPhoneFormat
            , PH.countryCode
              
            , BO.title
            , BO.author
            , BO.publisher

            , UN.prefix
            , UN.suffix
            , UN.name
              
            , CN.fact

            , SP.planet
            , SP.moon
            , SP.galaxy
            , SP.nebula
            , SP.starCluster
            , SP.constellation
            , SP.star
            , SP.agency
            , SP.distanceMeasurement
              
            , MU.instruments
              
            , PO.names
              
            , FO.ingredients
            , FO.spices
            , FO.measurements
            , FO.measurementSizes

            , SI.characters
              
            , DR.theDoctors
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True

