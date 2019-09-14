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
import qualified Faker.App as AP
import qualified Faker.Educator as ED
import Faker.Name
import Faker.Source
import qualified Faker.WorldCup as WC
import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate" $ do
    it "ruby code" $ do
      hworld <- generate helloWorldRuby
      hworld `shouldSatisfy` isText
    it "name with middle - ee" $ do
      ctries <-
        generateWithSettings
          (setLocale "ee" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Kajar Vaino Kõiv"
    it "name with middle - pak" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-PAK" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Mian Khan Nambardar"
    it "name with middle - au" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-AU" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBe` "Sarah Kirlin Jenkins"
    it "group A - WC" $ do
      ga <- generate WC.groupsGroupA
      ga `shouldBe` "Russia"
    it "courseNumber - Edu" $ do
      ga <- generate ED.tertiaryCourseNumber
      ga `shouldBe` "215"
    it "App author" $ do
      ga <-
        generateWithSettings (setDeterministic defaultFakerSettings) AP.author
      ga `shouldBe` "Steuber, Donnelly and Goldner"
