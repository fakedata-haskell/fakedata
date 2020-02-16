{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnNzTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Bank as BH
import qualified Faker.Team as TE
import qualified Faker.University as UN

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "en-NZ"          

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
    it "validates en-NZ locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.firstName
            , NA.lastName
            , NA.nameWithMiddle
            , NA.suffix
              
            , CO.suffix
              
            , IN.domainSuffix

            , FA.secondaryAddress
            , FA.postcode
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.communityPrefix
            , FA.communitySuffix
            , FA.city
            , FA.mailBox
            , FA.fullAddress
            , FA.community
            , FA.streetName

            , PH.formats
            , PH.cellPhoneFormat
              
            , UN.name
              
            , TE.sport
            , TE.name
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
