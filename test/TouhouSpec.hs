{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TouhouSpec where

import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Touhou
import Test.Hspec
import TestImport
import Faker.Internal

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Touhou" $ do
    it "positive" $ do
      item <- generate positive
      item `shouldSatisfy` isText
    it "negative" $ do
      item <- generate negative
      item `shouldSatisfy` isText
