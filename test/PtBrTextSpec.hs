{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PtBrTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Appliance as AP
import qualified Faker.Book as BO
import qualified Faker.Coin as CO
import qualified Faker.Color as CL
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Compass as CE
import qualified Faker.ElectricalComponents as EC
import qualified Faker.Food as FO
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Gender as GE
import qualified Faker.Internet as IN
import qualified Faker.Job as JO
import qualified Faker.Lorem as LO
import qualified Faker.Measurement as ME
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Science as SC
import qualified Faker.Space as SP
import qualified Faker.Team as TE
import qualified Faker.University as UN
import qualified Faker.Vehicle as VE

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "pt-BR"

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
    it "validates pt-BR locale" $ do
      let functions :: [Fake Text] =
            [ NA.lastName
            , NA.firstName
            , NA.prefix
            , NA.suffix
            , NA.name
            , NA.maleFirstName
            , NA.femaleFirstName
            , NA.nameWithMiddle
            , PH.formats
            , PH.cellPhoneFormat
            , TE.name
            , TE.sport
            , FO.ingredients
            , FO.spices
            , FO.measurements
            , FO.measurementSizes
            , VE.licensePlate
            , GE.types
            , GE.binaryTypes
            , FA.city
            , FA.country
            , FA.countryCode
            , FA.postcode
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.state
            , FA.stateAbbr
            , FA.streetName
            , CL.name
            , CO.suffix
            , CO.name
            , UN.prefix
            , UN.suffix
            , UN.name
            , IN.domainSuffix
            , IN.freeEmail
            , LO.words
            , JO.field
            , JO.seniority
            , JO.position
            , JO.keySkills
            , JO.employmentType
            , JO.educationLevel
            , JO.title
            , SC.element
            , SP.planet
            , SP.moon
            , SP.distanceMeasurement
            , EC.active
            , EC.passive
            , EC.electromechanical
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
