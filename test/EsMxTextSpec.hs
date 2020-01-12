{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EsMxTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Subscription as SU

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "es-MX"

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
    it "validates es-MX locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.prefix
            , NA.suffix
            , NA.name
            , NA.nameWithMiddle

            , IN.domainSuffix
            , IN.freeEmail

            , CO.suffix
            , CO.name
              
            -- , PH.formats
            -- , PH.cellPhoneFormat
              
            , FA.country
            , FA.buildingNumber
            , FA.secondaryAddress
            , FA.state
            , FA.postcode
            , FA.stateAbbr
            , FA.timeZone
            , FA.city
            , FA.streetName

            , SU.plans
            , SU.statuses
            , SU.paymentMethods
            , SU.subscriptionTerms
            , SU.paymentTerms
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
