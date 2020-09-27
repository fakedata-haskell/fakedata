{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ItTextSpec where

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
locale = "it"

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
    it "validates it locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.prefix
            , NA.name
            , NA.nameWithMiddle
              
            , PH.formats
            , PH.countryCode
              
            , SU.plans
            , SU.statuses
            , SU.paymentMethods
            , SU.subscriptionTerms
            , SU.paymentTerms

            , FA.cityPrefix
            , FA.citySuffix
            , FA.country
            , FA.city
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.streetName
            , FA.streetAddress
              
            , CO.suffix
            , CO.buzzword
            , CO.bs
            , CO.name
              
            , IN.freeEmail
            , IN.domainSuffix
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
