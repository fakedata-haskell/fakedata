{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CaCatTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

fakerSettings :: FakerSettings
fakerSettings = setLocale "ca-CAT" defaultFakerSettings

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map (generateWithSettings fakerSettings) funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "ca CAT Locale" $ do
      let functions :: [Fake Text] =
            [ 
            FA.city
            , FA.country
            , FA.buildingNumber
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.postcode
            , FA.state
            , FA.streetName
            , FA.streetAddress
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
