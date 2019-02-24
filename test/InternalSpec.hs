{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module InternalSpec where

import Faker.Internal
import Test.Hspec

spec :: Spec
spec = do
  describe "Internal module" $ do
    describe "refinedString" $ do
      it "basic check" $ do
        let str = "helloWorld"
        (refinedString str) `shouldBe` str
      it "removes unwanted char" $ do
        let str = "hello_World"
        (refinedString str) `shouldBe` "helloWorld"
      it "proper case conversion" $ do
        let str = "hello_world"
        (refinedString str) `shouldBe` "helloWorld"
