{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BirdSpec where

import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import Faker.Bird
import Test.Hspec
import TestImport
import Faker.Internal

isText :: Text -> Bool
isText x = T.length x >= 1

fakerSettings :: FakerSettings
fakerSettings = defaultFakerSettings

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map (generateWithSettings fakerSettings) funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "Bird" $ do
    it "sanity checking" $ do
      let functions :: [Fake Text] =
                       [
                        anatomy,
                        anatomyPastTense,
                        geo,
                        colors, emotionalAdjectives,
                        sillyAdjectives, adjectives, commonFamilyName,
                        -- plausibleCommonNames,
                        -- implausibleCommonNames,
                        orderCommonMapAccipitriformes,
                        orderCommonMapAnseriformes,
                        orderCommonMapApterygiformes,
                        orderCommonMapBucerotiformes,
                        orderCommonMapCaprimulgiformes,
                        orderCommonMapCariamiformes,
                        orderCommonMapCasuariiformes,
                        orderCommonMapCathartiformes,
                        orderCommonMapCharadriiformes,
                        orderCommonMapCiconiiformes,
                        orderCommonMapColiiformes,
                        orderCommonMapColumbiformes,
                        orderCommonMapCoraciiformes,
                        orderCommonMapCuculiformes,
                        orderCommonMapEurypygiformes,
                        orderCommonMapFalconiformes,
                        orderCommonMapGalbuliformes,
                        orderCommonMapGalliformes,
                        orderCommonMapGaviiformes,
                        orderCommonMapGruiformes,
                        orderCommonMapMesitornithiformes,
                        orderCommonMapMusophagiformes,
                        orderCommonMapOpisthocomiformes,
                        orderCommonMapOtidiformes,
                        orderCommonMapPasseriformes,
                        orderCommonMapPelecaniformes,
                        orderCommonMapPhaethontiformes,
                        orderCommonMapPhoenicopteriformes,
                        orderCommonMapPiciformes,
                        orderCommonMapPodicipediformes,
                        orderCommonMapProcellariiformes,
                        orderCommonMapPsittaciformes,
                        orderCommonMapPterocliformes,
                        orderCommonMapRheiformes,
                        orderCommonMapSphenisciformes,
                        orderCommonMapStrigiformes,
                        orderCommonMapStruthioniformes,
                        orderCommonMapSuliformes,
                        orderCommonMapTinamiformes,
                        orderCommonMapTrogoniformes
                       ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
