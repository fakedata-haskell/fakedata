{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HyTextSpec where

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
import qualified Faker.Commerce as CE
import qualified Faker.Compass as CE
import qualified Faker.Job as JO
import qualified Faker.Military as MI
import qualified Faker.Nation as NE
import qualified Faker.Artist as AR
import qualified Faker.Color as CL
import qualified Faker.Space as SP
import qualified Faker.Science as SC
import qualified Faker.Food as FO
import qualified Faker.Currency as CU

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "hy"

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
    it "validates hy locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.maleFirstName
            , NA.femaleFirstName
            , NA.name
            , NA.nameWithMiddle
              
            , NE.nationality
            , NE.language
            , NE.capitalCity
              
            , PH.formats
            , PH.cellPhoneFormat
            
            , MI.armyRank
            
            , AR.names

            , JO.field
            , JO.title
            , JO.seniority
            , JO.position
            , JO.keySkills
            , JO.employmentType
            , JO.educationLevel
 
            , IN.domainSuffix
            , IN.freeEmail
            
            , BO.title
            -- , BO.author
            , BO.publisher
            , BO.genre

            , CL.name
              
            , CE.department
            , CE.productNameAdjective
            , CE.productNameMaterial
            , CE.productNameProduct
            , CE.promotionCodeAdjective
            , CE.promotionCodeNoun
              
            , CO.suffix
            , CO.buzzword
            , CO.bs
            , CO.name
            , CO.industry
            , CO.profession
            , CO.type'

            , CE.cardinalWord
            , CE.cardinalAbbreviation
            , CE.ordinalWord
            , CE.ordinalAbbreviation
            , CE.halfWindWord
            , CE.halfWindAbbreviation
            , CE.abbreviation

            , CU.name
            , FO.dish
            , FO.descriptions
            , FO.ingredients
            , FO.fruits
            , FO.vegetables
            , FO.spices
            , FO.measurements
            , FO.metricMeasurements
              
            , SC.element
            , SC.scientist

            , SP.planet
            , SP.moon
            , SP.galaxy
            , SP.nebula
            , SP.constellation
            , SP.star
            , SP.distanceMeasurement
            , SP.meteorite
              
            , FA.country
            , FA.countryCode
            , FA.state
            , FA.stateAbbr
            , FA.community
            , FA.city
            , FA.cityPrefix
            , FA.postcode
            , FA.streetSuffix
            , FA.buildingNumber
            , FA.streetName
            , FA.streetAddress
            , FA.fullAddress
            , FA.secondaryAddress
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
