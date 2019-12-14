{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module BookSpec where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker
import Faker.Book
import Test.Hspec

isText :: Text -> Bool
isText x = T.length x >= 1

spec :: Spec
spec = do
  describe "Faker Generate for Book" $ do
    it "title" $ do
      item <- generate title
      item `shouldSatisfy` isText
    it "Author" $ do
      item <- generate author
      item `shouldSatisfy` isText
    it "Publisher" $ do
      item <- generate publisher
      item `shouldSatisfy` isText
    it "Genre" $ do
      item <- generate genre
      item `shouldSatisfy` isText
