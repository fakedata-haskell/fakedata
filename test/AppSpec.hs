{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Config
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.App
import Faker.Internal
import Faker.Provider.App
import Test.Hspec
import TestImport

spec :: Spec
spec = do
  describe "App" $ do
    it "Resolves App name" $ do
      ctries <- resolveAppField defaultFakerSettings "Name.name"
      ctries `shouldSatisfy` (\x -> T.length x >= 0)
    it "generates App name (sanity TH check)" $ do
      aname <- generate name
      aname `shouldBe` "Redhold"
    it "generates App name (TH check)" $ do
      let name2 :: Fake Text
          name2 = Fake $ resolver appNameProvider
      aname <- generate name
      bname <- generate name2
      aname `shouldBe` bname
    it "generates App name (TH check)" $ do
      let name2 :: Fake Text
          name2 = Fake $ resolver appNameProvider
      aname <- generate name
      bname <- generate name2
      aname `shouldBe` bname
