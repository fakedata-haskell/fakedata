{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module EnAuTextSpec where

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
import qualified Faker.Internet as IN
import qualified Faker.University as UN
import qualified Faker.Bank as BN
import Faker.Combinators (listOf)

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

fakerSettings :: FakerSettings
fakerSettings = setLocale "en-AU" defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates en-AU locale" $ do
      let functions :: [Fake Text] =
            [ 
              FA.state
            , FA.stateAbbr
            , FA.postcode
            , FA.buildingNumber
            , FA.streetSuffix

            , CO.suffix
              
            , IN.domainSuffix

            , NA.firstName
            , NA.lastName
            , NA.nameWithMiddle
            , NA.prefix
              
            , PH.formats
            , PH.cellPhoneFormat 
              
            , TE.sport
              
            , UN.name
            , BN.name
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True

