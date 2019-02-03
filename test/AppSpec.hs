{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Config
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker

-- import Faker.App
import Faker.Internal
import Faker.Provider.App
import Test.Hspec

spec :: Spec
spec = do
  describe "App" $ do
    it "Resolves App name" $ do
      ctries <- resolveAppField defaultFakerSettings "Name.name"
      ctries `shouldSatisfy` (\x -> T.length x >= 0)
    -- it "Resolves App name" $ do
    --   aname <- generate author
    --   aname `shouldBe` "test"
