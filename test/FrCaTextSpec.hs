{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FrCaTextSpec where

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

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "fr-CA"

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
    it "validates fr-CA locale" $ do
      let functions :: [Fake Text] =
            [ NA.lastName
            , NA.firstName
            , NA.prefix
            , NA.name
            , NA.nameWithMiddle
              
            , BO.title
            , BO.author
            , BO.publisher

            , PO.names
            , PO.locations
            , PO.moves

            , PH.formats

            , FA.state
            , FA.stateAbbr
            , FA.countryCode
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.streetName
            , FA.streetAddress
            , FA.postcode
              
            , IN.freeEmail
            , IN.domainSuffix

            , CO.suffix
            , CO.name
            , CO.buzzword
            , CO.bs

            , LO.words
            , LO.supplemental
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
