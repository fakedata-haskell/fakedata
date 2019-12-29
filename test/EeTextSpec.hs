{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EeTextSpec where

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
import Faker.Combinators (listOf)

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

fakerSettings :: FakerSettings
fakerSettings = setLocale "ee" defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates ee locale" $ do
      let functions :: [Fake Text] =
            [ 
              FA.postcode
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.state
            , FA.city
            , FA.streetName
            , FA.streetAddress

            , CO.suffix
            , CO.name
            , CO.buzzword
            , CO.bs
            , CO.name
              
            , IN.domainSuffix
            , IN.freeEmail

            , NA.firstName
            , NA.lastName
            , NA.name
            , NA.nameWithMiddle
              
            , PH.formats
            , PH.cellPhoneFormat
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True

