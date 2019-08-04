{-# LANGUAGE ScopedTypeVariables #-}

module TextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Ancient as AN
import qualified Faker.App as AP
import qualified Faker.Appliance as AP
import qualified Faker.Artist as AR
import qualified Faker.Bank as BA
import qualified Faker.Beer as BE
import qualified Faker.Book as BO
import qualified Faker.Book.CultureSeries as CE
import qualified Faker.Book.Dune as DU
import qualified Faker.Book.Lovecraft as LO
import qualified Faker.BossaNova as BO
import qualified Faker.Business as BU
import qualified Faker.Cannabis as CA
import qualified Faker.ChuckNorris as CH
import qualified Faker.Code as CO
import qualified Faker.Coffee as CO

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map generate funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "Address" $ do
      let functions :: [Fake Text] =
            [ FA.country
            , FA.cityPrefix
            , FA.citySuffix
            , FA.countryCode
            , FA.countryCodeLong
            , FA.buildingNumber
            , FA.communityPrefix
            , FA.communitySuffix
            , FA.community
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.timeZone
            , FA.city
            , FA.streetName
            , FA.streetAddress
            , FA.fullAddress
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Ancient" $ do
      let functions :: [Fake Text] = [AN.god, AN.primordial, AN.hero, AN.titan]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "App" $ do
      let functions :: [Fake Text] = [AP.name, AP.version]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Appliance" $ do
      let functions :: [Fake Text] = [AP.brand, AP.equipment]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Artist" $ do
      let functions :: [Fake Text] = [AR.names]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Bank" $ do
      let functions :: [Fake Text] = [BA.name, BA.swiftBic]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Beer" $ do
      let functions :: [Fake Text] =
            [BE.name, BE.brand, BE.hop, BE.yeast, BE.malt, BE.style]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.CultureSeries" $ do
      let functions :: [Fake Text] =
            [ CE.books
            , CE.cultureShips
            , CE.cultureShipClasses
            , CE.cultureShipClassAbvs
            , CE.civs
            , CE.planets
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.Dune" $ do
      let functions :: [Fake Text] =
            [ DU.characters
            , DU.titles
            , DU.planets
            , DU.quotesGuildNavigator
            , DU.quotesEmperor
            , DU.quotesPaul
            , DU.quotesThufir
            , DU.quotesJessica
            , DU.quotesIrulan
            , DU.quotesMohiam
            , DU.quotesGurney
            , DU.quotesLeto
            , DU.quotesStilgar
            , DU.quotesLietKynes
            , DU.quotesPardotKynes
            , DU.quotesBaronHarkonnen
            , DU.quotesPiter
            , DU.quotesAlia
            , DU.quotesMapes
            , DU.quotesDuncan
            , DU.quotesYueh
            , DU.sayingsBeneGesserit
            , DU.sayingsFremen
            , DU.sayingsMentat
            , DU.sayingsMuaddib
            , DU.sayingsOrangeCatholicBible
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.LoveCraft" $ do
      let functions :: [Fake Text] =
            [LO.fhtagn, LO.deity, LO.location, LO.tome, LO.words]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book" $ do
      let functions :: [Fake Text] =
            [BO.author, BO.title, BO.publisher, BO.genre]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "BossaNova" $ do
      let functions :: [Fake Text] = [BO.artists, BO.songs]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Business" $ do
      let functions :: [Fake Text] = [BU.creditCardNumbers, BU.creditCardTypes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cannabis" $ do
      let functions :: [Fake Text] =
            [ CA.strains
            , CA.cannabinoidAbbreviations
            , CA.cannabinoids
            , CA.terpenes
            , CA.medicalUses
            , CA.healthBenefits
            , CA.categories
            , CA.types
            , CA.buzzwords
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "ChuckNorris" $ do
      let functions :: [Fake Text] = [CH.fact]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Code" $ do
      let functions :: [Fake Text] = [CO.asin]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Coffee" $ do
      let functions :: [Fake Text] =
            [ CO.country
            , CO.variety
            , CO.intensifier
            , CO.body
            , CO.descriptor
            , CO.name1
            , CO.name2
            , CO.notes
            , CO.blendName
            -- , CO.regionsColombia
            -- , CO.regionsBrazil
            -- , CO.regionsSumatra
            -- , CO.regionsEthiopia
            -- , CO.regionsHonduras
            -- , CO.regionsUganda
            -- , CO.regionsMexico
              -- you still need to add and fix regionsColombia
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
