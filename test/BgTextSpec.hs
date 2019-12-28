{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BgTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Name as NA
import qualified Faker.Internet as IN

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

fakerSettings :: FakerSettings
fakerSettings = setLocale "bg" defaultFakerSettings

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map (generateWithSettings fakerSettings) funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

verifyFakeInt :: [Fake Int] -> IO [Bool]
verifyFakeInt funs = do
  let fs :: [IO Int] = map (generateWithSettings fakerSettings) funs
      gs :: [IO Bool] = map (\f -> (\x -> x >= 0) <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "Bg Locale" $ do
      let functions :: [Fake Text] =
            [ FA.country
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.postcode
            -- , FA.city
            -- , FA.streetName
            -- , FA.streetAddress
            , NA.maleFirstName
            , NA.femaleFirstName
            , NA.firstName
            -- , NA.name
            , IN.freeEmail
            , IN.domainSuffix
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
