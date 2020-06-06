{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module KoTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Commerce as CM
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
locale = "ko"

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
    it "validates ko locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.name
            , NA.nameWithMiddle
              
            , PH.formats

            , FA.citySuffix
            , FA.city
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.streetSuffix
            , FA.streetName
              
            , CO.suffix
            , CO.name
              
            , IN.freeEmail
            , IN.domainSuffix
              
            , LO.words

            , CM.department
            , CM.productNameAdjective
            , CM.productNameMaterial
            , CM.productNameProduct
            , CM.promotionCodeAdjective
            , CM.promotionCodeNoun
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
