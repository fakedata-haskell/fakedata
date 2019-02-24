{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FakerSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Combinators
import Faker.Compass
import Faker.Educator
import qualified Faker.ElderScrolls as ES
import Faker.ElectricalComponents
import Faker.Esport
import Faker.Fallout
import qualified Faker.FamilyGuy as FG
import Faker.File
import Faker.Food
import Faker.Internal
import Faker.Job (field)
import Faker.Nation (flagEmoji)
import Faker.PhoneNumber
import Faker.Provider.Educator
import qualified Faker.Science as FS
import Faker.StarWars
import qualified Faker.University as FU
import Faker.WorldCup
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate" $ do
    it "Nested unresolved field" $ do
      ctries <- generate tertiaryCourseNumber
      (ctries) `shouldBe` "215"
  describe "Faker Compass Generate" $ do
    it "Resolver check" $ do
      ctries <- generate direction
      (ctries) `shouldBe` "SE"
    it "Resolver check" $ do
      ctries <- generate $ listOf 5 direction
      (ctries) `shouldSatisfy` (\x -> Prelude.length x == 5)
    it "Elder Scroll" $ do
      ctries <- generate ES.firstName
      (ctries) `shouldSatisfy` isText
    it "Electrical component" $ do
      ctries <- generate passive
      (ctries) `shouldSatisfy` isText
    it "Esport" $ do
      ctries <- generate leagues
      (ctries) `shouldSatisfy` isText
    it "Fallout" $ do
      ctries <- generate factions
      ctries `shouldSatisfy` isText
    it "Family Guy" $ do
      ctries <- generate FG.quote
      ctries `shouldSatisfy` isText
    it "File" $ do
      ctries <- generate mimeType
      ctries `shouldSatisfy` isText
    it "Food" $ do
      ctries <- generate dish
      ctries `shouldSatisfy` isText
    it "Science" $ do
      item <- generate FS.element
      item `shouldSatisfy` isText
    it "StarWars" $ do
      item <- generate callNumbers
      item `shouldSatisfy` isText
    it "StarWars (2)" $ do
      item <- generate callSign
      item `shouldSatisfy` isText
    describe "Phone number" $ do
      it "countryCode" $ do
        item <- generate countryCode
        item `shouldSatisfy` isText
      it "cellPhoneFormat" $ do
        item <- generate cellPhoneFormat
        item `shouldSatisfy` isText
      it "formats" $ do
        item <- generate formats
        item `shouldSatisfy` isText
    it "University" $ do
      item <- generate FU.name
      item `shouldSatisfy` isText
    it "Nation - flagEmoji" $ do
      item <- generate flagEmoji
      item `shouldSatisfy` isText
    it "Job" $ do
      item <- generate field
      item `shouldSatisfy` isText
    describe "WorldCup" $ do
      it "SouthKorea" $ do
        item <- generate rostersSouthKoreaCoach
        print item
        item `shouldSatisfy` isText
