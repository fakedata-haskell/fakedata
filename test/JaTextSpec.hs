{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module JaTextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import Faker.Combinators (listOf)
import qualified Faker.Company as CO
import qualified Faker.Internet as IN
import qualified Faker.Name as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.Book as BO
import qualified Faker.Lorem as LO
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Appliance as AP
import qualified Faker.Measurement as ME
import qualified Faker.Compass as CE
import qualified Faker.Color as CL
import qualified Faker.University as UN
import qualified Faker.Space as SP
import Faker.Ancient
import Faker.Food
import Faker.Game.Zelda
import Faker.Gender
import Faker.Creature.Cat
import Faker.Creature.Dog
import Faker.Creature.Animal
import qualified Faker.Restaurant as RE
import Faker.Bank
import Faker.Coffee
import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

isTexts :: [Text] -> Bool
isTexts xs = and $ map isText xs

locale :: Text
locale = "ja"

fakerSettings :: FakerSettings
fakerSettings = setLocale locale defaultFakerSettings

verifyDistributeFakes :: [Fake Text] -> IO [Bool]
verifyDistributeFakes funs = do
  let fs :: [IO [Text]] =
        map (generateWithSettings fakerSettings) $ map (listOf 100) funs
      gs :: [IO Bool] = map (\f -> isTexts <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "validates ja locale" $ do
      let functions :: [Fake Text] =
            [ 
              NA.lastName
            , NA.firstName
            , NA.name
            , NA.nameWithMiddle
              
            , RE.namePrefix
            , RE.nameSuffix
            , RE.name
            , RE.type'
              
            , PH.formats
            , PH.cellPhoneFormat
            , CL.name
            , Faker.Coffee.country

            , SP.planet
            , SP.galaxy
              
            , UN.prefix
            , UN.suffix
            , UN.name

            , FA.cityPrefix
            , FA.citySuffix
            , FA.city
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.streetName
              
            , Faker.Bank.name
            , Faker.Creature.Animal.name
            , Faker.Creature.Cat.breed
            , Faker.Creature.Dog.breed
            , Faker.Ancient.god
              
            , CO.suffix
            , CO.name

            , Faker.Food.sushi

            , PO.names
            , PO.locations
            , PO.moves
            
            , Faker.Game.Zelda.games
            , Faker.Gender.binaryTypes

            , LO.words
            , LO.supplemental
            ]
      bools <- verifyDistributeFakes functions
      (and bools) `shouldBe` True
