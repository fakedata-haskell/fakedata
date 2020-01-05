{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module SvTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Commerce as CE
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Book as BO
import qualified Faker.Lorem as LO
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Appliance as AP
import qualified Faker.Measurement as ME
import qualified Faker.Compass as CE
import qualified Faker.Coin as CO
import qualified Faker.Color as CL
import qualified Faker.Gender as GE
import qualified Faker.Vehicle as VE
import qualified Faker.Food as FO
import qualified Faker.Team as TE
import qualified Faker.Job as JO
import qualified Faker.University as UN

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "sv"

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
    it "validates sv locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.maleFirstName
            , NA.femaleFirstName
            , NA.prefix
            , NA.firstName
            , NA.name

            , PH.formats
            , PH.cellPhoneFormat

            , FA.cityPrefix
            , FA.citySuffix
            , FA.city
            , FA.streetName
            , FA.streetAddress
            , FA.country
            , FA.streetSuffix
            , FA.postcode
            , FA.buildingNumber
            , FA.secondaryAddress
            , FA.state
              
            , IN.domainSuffix

            , CO.suffix
            , CO.name
              
            , CE.department
            , CE.productNameAdjective
            , CE.productNameProduct
            , CE.productNameMaterial

            -- , TE.name
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
