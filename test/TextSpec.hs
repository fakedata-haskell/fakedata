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
import qualified Faker.Coin as CO
import qualified Faker.Color as CO
import qualified Faker.Commerce as CO
import qualified Faker.Company as CM
import qualified Faker.Compass as CO
import qualified Faker.Construction as CO
import qualified Faker.Cosmere as CO
import qualified Faker.Creature.Animal as AN
import qualified Faker.Creature.Cat as CA
import qualified Faker.Creature.Dog as DO
import qualified Faker.Creature.Horse as HO
import qualified Faker.CryptoCoin as CO
import qualified Faker.Currency as CU
import qualified Faker.DcComics as DC

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
            , CO.regionsColombia
            , CO.regionsBrazil
            , CO.regionsSumatra
            , CO.regionsEthiopia
            , CO.regionsHonduras
            , CO.regionsKenya
            , CO.regionsUganda
            , CO.regionsMexico
            , CO.regionsGuatemala
            , CO.regionsNicaragua
            , CO.regionsCostaRica
            , CO.regionsTanzania
            , CO.regionsElSalvador
            , CO.regionsRwanda
            , CO.regionsBurundi
            , CO.regionsPanama
            , CO.regionsYemen
            , CO.regionsIndia
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Coin" $ do
      let functions :: [Fake Text] = [CO.flip]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Color" $ do
      let functions :: [Fake Text] = [CO.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Commerce" $ do
      let functions :: [Fake Text] =
            [ CO.department
            , CO.productNameAdjective
            , CO.productNameMaterial
            , CO.productNameProduct
            , CO.promotionCodeAdjective
            , CO.promotionCodeNoun
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Company" $ do
      let functions :: [Fake Text] =
            [ CM.suffix
            , CM.buzzword
            , CM.bs
            , CM.name
            , CM.industry
            , CM.profession
            , CM.type'
            , CM.sicCode
              -- Fix buzzword and bs TOD
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Company" $ do
      let functions :: [Fake Text] =
            [ CO.direction
            , CO.abbreviation
            , CO.azimuth
            , CO.cardinalWord
            , CO.cardinalAbbreviation
            , CO.cardinalAzimuth
            , CO.ordinalWord
            , CO.ordinalAbbreviation
            , CO.ordinalAzimuth
            , CO.halfWindWord
            , CO.halfWindAbbreviation
            , CO.halfWindAzimuth
            , CO.quarterWindWord
            , CO.quarterWindAbbreviation
            , CO.quarterWindAzimuth
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Construction" $ do
      let functions :: [Fake Text] =
            [ CO.materials
            , CO.subcontractCategories
            , CO.heavyEquipment
            , CO.roles
            , CO.trades
            , CO.standardCostCodes
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cosmere" $ do
      let functions :: [Fake Text] =
            [ CO.aons
            , CO.shardWorlds
            , CO.shards
            , CO.surges
            , CO.knightsRadiant
            , CO.metals
            , CO.allomancers
            , CO.feruchemists
            , CO.heralds
            , CO.sprens
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Animal" $ do
      let functions :: [Fake Text] = [AN.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Cat" $ do
      let functions :: [Fake Text] = [CA.name, CA.breed, CA.registry]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Dog" $ do
      let functions :: [Fake Text] =
            [ DO.name
            , DO.breed
            , DO.sound
            , DO.memePhrase
            , DO.age
            , DO.coatLength
            , DO.size
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Horuse" $ do
      let functions :: [Fake Text] = [HO.name, HO.breed]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cryptocoin" $ do
      let functions :: [Fake Text] = [CO.coin]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Currency" $ do
      let functions :: [Fake Text] = [CU.name, CU.code, CU.symbol]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "DcComics" $ do
      let functions :: [Fake Text] =
            [DC.hero, DC.heroine, DC.villain, DC.name, DC.title]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
