{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CombinatorsSpec where

import Data.Text hiding (all, map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker
import Faker.Address
import Test.Hspec
import Faker.Combinators
import Data.Maybe (isJust)
import Data.List (length)

spec :: Spec
spec = do
  describe "Combinators" $ do
    it "fromRange" $ do
      let t = fromRange (0, 9)
      item :: Int <- generate t
      item `shouldSatisfy` (\x -> x >= 0)
    it "pickAny" $ do
      item :: Word <- generate pickAny
      item `shouldSatisfy` (\x -> x >= 0)
    it "suchThat" $ do
      item :: Text <- generate $ suchThat country (\x -> (T.length x > 2))
      item `shouldSatisfy` (\x -> T.length x > 3)
    it "suchThatMaybe" $ do
      item :: Maybe Text <- generate $ suchThatMaybe country (\x -> T.length x > 2)
      item `shouldSatisfy` isJust
    it "oneOf" $ do
      item :: Text <- generate $ oneof [country, citySuffix]
      item `shouldSatisfy` (\x -> T.length x > 3)
    it "listOf" $ do
      item :: [Text] <- generate $ listOf 5 country
      item `shouldSatisfy` (\x -> Prelude.length x == 5)
    it "orderedList" $ do
      item :: [Text] <- generate $ orderedList 5 country
      item `shouldSatisfy` (\x -> Prelude.length x == 5)
