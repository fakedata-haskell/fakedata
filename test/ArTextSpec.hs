{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ArTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Company as CO
import qualified Faker.Color as CL
import qualified Faker.Name as NA
import qualified Faker.Commerce as CE
import qualified Faker.Team as TE
import qualified Faker.App as AP
import qualified Faker.Book as BO

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

fakerSettings :: FakerSettings
fakerSettings = setLocale "ar" defaultFakerSettings

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
    it "Address" $ do
      let functions :: [Fake Text] =
            [ FA.country
            , FA.cityPrefix
            , FA.secondaryAddress
            , FA.postcode
            , FA.city
            , FA.cityPrefix
            , FA.streetName
            , FA.buildingNumber
            , FA.streetAddress
            , FA.fullAddress
            , CO.suffix
            , CO.buzzword
            , CO.bs
            , CO.name
            , CO.industry
            , CO.profession
            , NA.firstName
            , NA.lastName
            , NA.prefix
            , NA.name
            , NA.nameWithMiddle
            , CL.name
            , CE.department
            , CE.productNameAdjective
            , CE.productNameMaterial
            , CE.productNameProduct
            , CE.promotionCodeAdjective
            , CE.promotionCodeNoun
            , TE.creature
            , TE.name
            , TE.sport
            , AP.name
            , AP.version
            , AP.author
            , BO.title
            , BO.author
            , BO.publisher
            , BO.genre
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
