{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module GamesSpec where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import qualified Faker.Game.Dota as GD
import qualified Faker.Game.ElderScrolls as GE
import qualified Faker.Game.Fallout as GF
import qualified Faker.Game.HalfLife as GH
import qualified Faker.Game.Heroes as GHH
import qualified Faker.Game.HeroesOfTheStorm as GHT
import qualified Faker.Game.Myst as GM
import qualified Faker.Game.Overwatch as GO
import qualified Faker.Game.Pokemon as GP
import qualified Faker.Game.SonicTheHedgehog as GS
import qualified Faker.Game.SuperSmashBros as GSS
import qualified Faker.Game.Witcher as GW
import qualified Faker.Game.WorldOfWarcraft as GWOW
import qualified Faker.Game.Zelda as GZ
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate for Games" $ do
    it "Dota" $ do
      item <- generate GD.hero
      item `shouldSatisfy` isText
    it "Dota Quote" $ do
      item <- generate GD.meepoQuote
      item `shouldSatisfy` isText
    it "ElderScrolls" $ do
      item <- generate GE.race
      item `shouldSatisfy` isText
    it "Fallout" $ do
      item <- generate GF.locations
      item `shouldSatisfy` isText
    it "Fallout" $ do
      item <- generate GF.locations
      item `shouldSatisfy` isText
    it "HalfLife" $ do
      item <- generate GH.enemy
      item `shouldSatisfy` isText
    it "Heroes" $ do
      item <- generate GHH.klasses
      item `shouldSatisfy` isText
    it "Heroes of the Storm" $ do
      item <- generate GHT.heroes
      item `shouldSatisfy` isText
    it "Myst" $ do
      item <- generate GM.games
      item `shouldSatisfy` isText
    it "Overwatch" $ do
      item <- generate GO.heroes
      item `shouldSatisfy` isText
    it "Pokemon" $ do
      item <- generate GP.names
      item `shouldSatisfy` isText
    it "SonicTheHedgehog" $ do
      item <- generate GS.game
      item `shouldSatisfy` isText
    it "SuperSmashBros" $ do
      item <- generate GSS.stage
      item `shouldSatisfy` isText
    it "Witcher" $ do
      item <- generate GW.monsters
      item `shouldSatisfy` isText
    it "WorldOfWarcraft" $ do
      item <- generate GWOW.heros
      item `shouldSatisfy` isText
    it "Zelda" $ do
      item <- generate GZ.items
      item `shouldSatisfy` isText
