{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FakerSpec where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Combinators
import Faker.Company
import Faker.Compass
import Faker.Construction
import Faker.Creature.Horse
import Faker.DateTime
import Faker.Educator
import Faker.ElectricalComponents
import Faker.Esport
import Faker.File
import Faker.Finance
import Faker.Food
import qualified Faker.Game.ElderScrolls as ES
import Faker.Game.Fallout
import Faker.Internal
import Faker.Job (field)
import Faker.Movie.StarWars
import Faker.PhoneNumber
import qualified Faker.Restaurant as FR
import qualified Faker.Science as FS
import qualified Faker.Sport.Basketball as FB
import qualified Faker.TvShow.FamilyGuy as FG
import qualified Faker.University as FU
import qualified Faker.Vehicle as FV
import Faker.WorldCup
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

isDay :: Day -> Bool
isDay _ = True

isUTCTime :: UTCTime -> Bool
isUTCTime _ = True

spec :: Spec
spec = do
  describe "Faker Generate" $ do
    it "Nested unresolved field" $ do
      ctries <- generate tertiaryCourseNumber
      (ctries) `shouldBe` "215"
  describe "Faker Compass Generate" $ do
    it "Resolver check" $ do
      ctries <- generate direction
      (ctries) `shouldBe` "southwest"
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
    -- it "Nation - flagEmoji" $ do
    --   item <- generate flagEmoji
    --   item `shouldSatisfy` isText
    it "Job" $ do
      item <- generate field
      item `shouldSatisfy` isText
    describe "WorldCup" $ do
      it "SouthKorea" $ do
        item <- generate rostersSouthKoreaCoach
        item `shouldSatisfy` isText
    describe "Restaurant" $ do
      it "name" $ do
        item <- generate FR.name
        item `shouldBe` "Sweet House"
      it "name_suffix" $ do
        item <- generate FR.nameSuffix
        item `shouldSatisfy` isText
      it "type'" $ do
        item <- generate FR.type'
        item `shouldBe` "Bakery"
    describe "Vehicle" $ do
      it "license" $ do
        item <- generate FV.licensePlate
        item `shouldSatisfy` isText
    describe "Construction" $ do
      it "roles" $ do
        item <- generate roles
        item `shouldSatisfy` isText
    describe "Basketball" $ do
      it "roles" $ do
        item <- generate FB.coaches
        item `shouldSatisfy` isText
    describe "Company" $ do
      it "bs" $ do
        item <- generate bs
        item `shouldBe` "visualize efficient supply-chains"
      it "buzzwords" $ do
        item <- generate buzzword
        item `shouldBe` "info-mediaries"
      it "sicCode" $ do
        item <- generate sicCode
        item `shouldSatisfy` isText
    describe "Creatures" $ do
      it "horse" $ do
        item <- generate breed
        item `shouldSatisfy` isText
    describe "DateTime" $ do
      it "day" $ do
        item <- generate day
        item `shouldSatisfy` isDay
    describe "Finance" $ do
      it "visa" $ do
        item <- generate visa
        item `shouldSatisfy` isText
      it "mastercard" $ do
        item <- generate mastercard
        item `shouldSatisfy` isText
