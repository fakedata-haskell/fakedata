{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EsArTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Compass as CO
import qualified Faker.Company as CY
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.Team as TE
import qualified Faker.Military as MI
import qualified Faker.Sport.Football as FO
import qualified Faker.Sport.Basketball as BA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Bank as BA
import qualified Faker.Color as CU
import qualified Faker.Currency as CR
import qualified Faker.Nation as NA

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "es-AR"

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
    it "validates es locale" $ do
      let functions :: [Fake Text] =
            [ NA.maleFirstName,
              NA.femaleFirstName,
              NA.firstName,
              NA.lastName,
              NA.suffix,
              NA.name,
              NA.nameWithMiddle,
              FA.cityPrefix,
              FA.citySuffix,
              FA.city,
              FA.state,
              FA.stateAbbr,
              FA.cityWithState,
              FA.buildingNumber,
              FA.secondaryAddress,
              FA.streetAddress,
              FA.postcode,
              FA.fullAddress,
              FA.country,
              FO.teams,
              FO.players,
              FO.coaches,
              FO.competitions,
              FO.positions,
              BA.teams,
              BA.players,
              BA.coaches,
              BA.positions,
              TE.sport,
              TE.name,
              MI.armyRank,
              MI.navyRank,
              MI.coastGuardRank,
              MI.airForceRank,
              CO.cardinalWord,
              CO.cardinalAbbreviation,
              CO.ordinalWord,
              CO.ordinalAbbreviation,
              CO.halfWindWord,
              CO.halfWindAbbreviation,
              CO.quarterWindWord,
              CO.quarterWindAbbreviation,
              PH.formats,
              PH.cellPhoneFormat,
              PH.countryCode,
              BA.name,
              CU.name,
              CR.name,
              NA.nationality,
              NA.capitalCity,
              NA.language,
              CY.suffix,
              CY.bs,
              CY.name,
              CY.industry,
              CY.profession,
              CY.type'
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
