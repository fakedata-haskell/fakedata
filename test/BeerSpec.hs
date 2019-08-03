{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BeerSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Internal
import Faker.Provider.Beer
import Test.Hspec
import TestImport

spec :: Spec
spec = do
  describe "Beer" $ do
    it "Has enough beers" $ do
      ctries <- beerNameProvider defaultFakerSettings
      (ctries) `shouldSatisfy` \x -> V.length x > 10
    it "Equality of generated and manually coded beers" $ do
      let beerNameProvider2 ::
               (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
          beerNameProvider2 settings = fetchData settings Beer parseBeerName
      beers1 <- beerNameProvider2 defaultFakerSettings
      beers2 <- beerNameProvider defaultFakerSettings
      beers1 `shouldBe` beers2
    it "Equality of generated and manually coded beer brands" $ do
      let beerBrandProvider2 ::
               (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
          beerBrandProvider2 settings = fetchData settings Beer parseBeerBrand
      beers1 <- beerBrandProvider2 defaultFakerSettings
      beers2 <- beerBrandProvider defaultFakerSettings
      beers1 `shouldBe` beers2
