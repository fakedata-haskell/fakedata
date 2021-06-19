{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module OperaSpec where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Music.Opera
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate for Opera" $ do
    it "italianByGiuseppeVerdi" $ do
      item <- generate italianByGiuseppeVerdi
      item `shouldSatisfy` isText
    it "italianByGioacchinoRossini" $ do
      item <- generate italianByGioacchinoRossini
      item `shouldSatisfy` isText
    it "italianByGaetanoDonizetti" $ do
      item <- generate italianByGaetanoDonizetti
      item `shouldSatisfy` isText
    it "italianByVincenzoBellini" $ do
      item <- generate italianByVincenzoBellini
      item `shouldSatisfy` isText
    it "todo" $ do
      item <- generate undefined
      item `shouldSatisfy` isText
