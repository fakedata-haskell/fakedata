{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OtherSpec where

import Control.Applicative ((<|>))
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Semigroup ((<>))
import Data.Text hiding (all, map)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Faker hiding (defaultFakerSettings)
import qualified Faker.App as AP
import qualified Faker.Educator as ED
import Faker.Name
import qualified Faker.Address as AD
import Faker.Source
import qualified Faker.WorldCup as WC
import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

data Person = Person {
    personName :: Text,
    personAddress :: Text
} deriving (Show, Eq)

fakePerson :: Fake Person
fakePerson = do
    personName <- name
    personAddress <- AD.fullAddress
    pure $ Person{..}

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
      (ctries) `shouldBeOneOf` ["Kaido Välbe Mark", "Kardo Salum\228e Levandi"]
    it "name with middle - pak" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-PAK" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBeOneOf` ["Zia Butt Iqbal", "Taj Mughal Hussain"]
    it "name with middle - au" $ do
      ctries <-
        generateWithSettings
          (setLocale "en-AU" defaultFakerSettings)
          nameWithMiddle
      (ctries) `shouldBeOneOf` ["Georgia Sarah Kelly", "Jasmine Alana Bergstrom"]
    it "group A - WC" $ do
      ga <- generate WC.groupsGroupA
      ga `shouldBeOneOf` ["Russia", "Uruguay"]
    it "App author" $ do
      ga <-
        generateWithSettings (setDeterministic defaultFakerSettings) AP.author
      ga `shouldBeOneOf` ["Schmidt, Breitenberg and Lowe", "Goldner, Will and Muller"]
    it "Same Person generated" $ do
      p1 <- generate fakePerson
      p2 <- generate fakePerson
      p1 `shouldBe` p2
    it "Different Person generated" $ do
      p1 <- generate fakePerson
      p2 <- generateNonDeterministic fakePerson
      p1 `shouldNotBe` p2
  describe "Semigroup instance of Fake" $
    it "can be appended and it is associative" $ do
      phraseL <- generate $ (pure "Hello " <> name) <> pure "!"
      phraseR <- generate $ pure "Hello " <> (name <> pure "!")
      let expected = ["Hello Brent Breitenberg!", "Hello Valentine Will V!"]
      phraseL `shouldBeOneOf` expected
      phraseR `shouldBeOneOf` expected
  describe "Monoid instance of Fake" $
    it "mappend mempty doesn't modify the other operand" $ do
      name' <- generate $ name `mappend` mempty
      name' `shouldBeOneOf` ["Brent Breitenberg", "Valentine Will V"]
