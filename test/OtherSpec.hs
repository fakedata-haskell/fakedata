{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module OtherSpec where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Name
import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate" $ do
    it "name with middle - ee" $ do
      ctries <-
        generateWithSettings
          (setLocale "ee" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Kaido Leok Leok"
    it "name with middle - pak" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-PAK" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Zia Ash Ash"
    it "name with middle - au" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-AU" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Georgia Grady Grady"
