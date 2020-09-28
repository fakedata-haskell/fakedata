{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnInTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

fakerSettings :: FakerSettings
fakerSettings = setLocale "en-IND" defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates en-IND locale" $ do
      let functions :: [Fake Text] =
            [ NA.firstName
            , NA.lastName
            , NA.nameWithMiddle
            , FA.postcode
            , FA.city
            , FA.state
            , FA.stateAbbr
            , IN.domainSuffix
            , IN.freeEmail
            , CO.suffix
            , PH.formats
            , PH.countryCode
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
