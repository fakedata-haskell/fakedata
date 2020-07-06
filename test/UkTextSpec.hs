{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module UkTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Commerce as CE
import qualified Faker.Artist as AR
import qualified Faker.Music as MU
import qualified Faker.Yoda as YO
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
locale = "uk"

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
    it "validates uk locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.firstName
            , NA.lastName
            , NA.maleFirstName
            , NA.femaleFirstName
            , NA.name
            , NA.nameWithMiddle
              
            , IN.freeEmail
            , IN.domainSuffix
              
            , PH.formats
            , PH.cellPhoneFormat
              
            , FA.city
            , FA.country
            , FA.buildingNumber
            -- , FA.streetSuffix (Data source is empty)
            , FA.postcode
            , FA.secondaryAddress
            , FA.state
            -- , FA.stateAbbr (Data source is empty)
            -- , FA.cityPrefix (Data source is empty)
            -- , FA.citySuffix (Data source is empty)
            , FA.streetName
            , FA.streetAddress
              
            , CE.department
            , CE.productNameAdjective
            , CE.productNameProduct
            , CE.productNameMaterial

            , CO.suffix
            , CO.name

            , AR.names
            , MU.instruments
            , YO.quotes
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
