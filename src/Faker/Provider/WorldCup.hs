{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.WorldCup where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseWorldCup :: FromJSON a => FakerSettings -> Value -> Parser a
parseWorldCup settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  worldCup <- faker .: "world_cup"
  pure worldCup
parseWorldCup settings val = fail $ "expected Object, but got " <> (show val)

parseWorldCupField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseWorldCupField settings txt val = do
  worldCup <- parseWorldCup settings val
  field <- worldCup .:? txt .!= mempty
  pure field

parseWorldCupFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseWorldCupFields settings txts val = do
  worldCup <- parseWorldCup settings val
  helper worldCup txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "worldCup" "teams")

$(genProvider "worldCup" "teams")

$(genParser "worldCup" "stadiums")

$(genProvider "worldCup" "stadiums")

$(genParser "worldCup" "cities")

$(genProvider "worldCup" "cities")

$(genParsers "worldCup" ["groups", "group_A"])

$(genProviders "worldCup" ["groups", "group_A"])

$(genParsers "worldCup" ["groups", "group_B"])

$(genProviders "worldCup" ["groups", "group_B"])

$(genParsers "worldCup" ["groups", "group_C"])

$(genProviders "worldCup" ["groups", "group_C"])

$(genParsers "worldCup" ["groups", "group_D"])

$(genProviders "worldCup" ["groups", "group_D"])

$(genParsers "worldCup" ["groups", "group_E"])

$(genProviders "worldCup" ["groups", "group_E"])

$(genParsers "worldCup" ["groups", "group_F"])

$(genProviders "worldCup" ["groups", "group_F"])

$(genParsers "worldCup" ["groups", "group_G"])

$(genProviders "worldCup" ["groups", "group_G"])

$(genParsers "worldCup" ["groups", "group_H"])

$(genProviders "worldCup" ["groups", "group_H"])

$(genParsers "worldCup" ["rosters", "Egypt", "coach"])

$(genProviders "worldCup" ["rosters", "Egypt", "coach"])

$(genParsers "worldCup" ["rosters", "Egypt", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Egypt", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Egypt", "defenders"])

$(genProviders "worldCup" ["rosters", "Egypt", "defenders"])

$(genParsers "worldCup" ["rosters", "Egypt", "midfielders"])

$(genProviders "worldCup" ["rosters", "Egypt", "midfielders"])

$(genParsers "worldCup" ["rosters", "Egypt", "forwards"])

$(genProviders "worldCup" ["rosters", "Egypt", "forwards"])

$(genParsers "worldCup" ["rosters", "Russia", "coach"])

$(genProviders "worldCup" ["rosters", "Russia", "coach"])

$(genParsers "worldCup" ["rosters", "Russia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Russia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Russia", "defenders"])

$(genProviders "worldCup" ["rosters", "Russia", "defenders"])

$(genParsers "worldCup" ["rosters", "Russia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Russia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Russia", "forwards"])

$(genProviders "worldCup" ["rosters", "Russia", "forwards"])

$(genParsers "worldCup" ["rosters", "Saudi_Arabia", "coach"])

$(genProviders "worldCup" ["rosters", "Saudi_Arabia", "coach"])

$(genParsers "worldCup" ["rosters", "Saudi_Arabia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Saudi_Arabia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Saudi_Arabia", "defenders"])

$(genProviders "worldCup" ["rosters", "Saudi_Arabia", "defenders"])

$(genParsers "worldCup" ["rosters", "Saudi_Arabia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Saudi_Arabia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Saudi_Arabia", "forwards"])

$(genProviders "worldCup" ["rosters", "Saudi_Arabia", "forwards"])

$(genParsers "worldCup" ["rosters", "Uruguay", "coach"])

$(genProviders "worldCup" ["rosters", "Uruguay", "coach"])

$(genParsers "worldCup" ["rosters", "Uruguay", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Uruguay", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Uruguay", "defenders"])

$(genProviders "worldCup" ["rosters", "Uruguay", "defenders"])

$(genParsers "worldCup" ["rosters", "Uruguay", "midfielders"])

$(genProviders "worldCup" ["rosters", "Uruguay", "midfielders"])

$(genParsers "worldCup" ["rosters", "Uruguay", "forwards"])

$(genProviders "worldCup" ["rosters", "Uruguay", "forwards"])

$(genParsers "worldCup" ["rosters", "Iran", "coach"])

$(genProviders "worldCup" ["rosters", "Iran", "coach"])

$(genParsers "worldCup" ["rosters", "Iran", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Iran", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Iran", "defenders"])

$(genProviders "worldCup" ["rosters", "Iran", "defenders"])

$(genParsers "worldCup" ["rosters", "Iran", "midfielders"])

$(genProviders "worldCup" ["rosters", "Iran", "midfielders"])

$(genParsers "worldCup" ["rosters", "Iran", "forwards"])

$(genProviders "worldCup" ["rosters", "Iran", "forwards"])

$(genParsers "worldCup" ["rosters", "Morocco", "coach"])

$(genProviders "worldCup" ["rosters", "Morocco", "coach"])

$(genParsers "worldCup" ["rosters", "Morocco", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Morocco", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Morocco", "defenders"])

$(genProviders "worldCup" ["rosters", "Morocco", "defenders"])

$(genParsers "worldCup" ["rosters", "Morocco", "midfielders"])

$(genProviders "worldCup" ["rosters", "Morocco", "midfielders"])

$(genParsers "worldCup" ["rosters", "Morocco", "forwards"])

$(genProviders "worldCup" ["rosters", "Morocco", "forwards"])

$(genParsers "worldCup" ["rosters", "Portugal", "coach"])

$(genProviders "worldCup" ["rosters", "Portugal", "coach"])

$(genParsers "worldCup" ["rosters", "Portugal", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Portugal", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Portugal", "defenders"])

$(genProviders "worldCup" ["rosters", "Portugal", "defenders"])

$(genParsers "worldCup" ["rosters", "Portugal", "midfielders"])

$(genProviders "worldCup" ["rosters", "Portugal", "midfielders"])

$(genParsers "worldCup" ["rosters", "Portugal", "forwards"])

$(genProviders "worldCup" ["rosters", "Portugal", "forwards"])

$(genParsers "worldCup" ["rosters", "Spain", "coach"])

$(genProviders "worldCup" ["rosters", "Spain", "coach"])

$(genParsers "worldCup" ["rosters", "Spain", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Spain", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Spain", "defenders"])

$(genProviders "worldCup" ["rosters", "Spain", "defenders"])

$(genParsers "worldCup" ["rosters", "Spain", "midfielders"])

$(genProviders "worldCup" ["rosters", "Spain", "midfielders"])

$(genParsers "worldCup" ["rosters", "Spain", "forwards"])

$(genProviders "worldCup" ["rosters", "Spain", "forwards"])

$(genParsers "worldCup" ["rosters", "Australia", "coach"])

$(genProviders "worldCup" ["rosters", "Australia", "coach"])

$(genParsers "worldCup" ["rosters", "Australia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Australia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Australia", "defenders"])

$(genProviders "worldCup" ["rosters", "Australia", "defenders"])

$(genParsers "worldCup" ["rosters", "Australia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Australia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Australia", "forwards"])

$(genProviders "worldCup" ["rosters", "Australia", "forwards"])

$(genParsers "worldCup" ["rosters", "Denmark", "coach"])

$(genProviders "worldCup" ["rosters", "Denmark", "coach"])

$(genParsers "worldCup" ["rosters", "Denmark", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Denmark", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Denmark", "defenders"])

$(genProviders "worldCup" ["rosters", "Denmark", "defenders"])

$(genParsers "worldCup" ["rosters", "Denmark", "midfielders"])

$(genProviders "worldCup" ["rosters", "Denmark", "midfielders"])

$(genParsers "worldCup" ["rosters", "Denmark", "forwards"])

$(genProviders "worldCup" ["rosters", "Denmark", "forwards"])

$(genParsers "worldCup" ["rosters", "France", "coach"])

$(genProviders "worldCup" ["rosters", "France", "coach"])

$(genParsers "worldCup" ["rosters", "France", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "France", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "France", "defenders"])

$(genProviders "worldCup" ["rosters", "France", "defenders"])

$(genParsers "worldCup" ["rosters", "France", "midfielders"])

$(genProviders "worldCup" ["rosters", "France", "midfielders"])

$(genParsers "worldCup" ["rosters", "France", "forwards"])

$(genProviders "worldCup" ["rosters", "France", "forwards"])

$(genParsers "worldCup" ["rosters", "Peru", "coach"])

$(genProviders "worldCup" ["rosters", "Peru", "coach"])

$(genParsers "worldCup" ["rosters", "Peru", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Peru", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Peru", "defenders"])

$(genProviders "worldCup" ["rosters", "Peru", "defenders"])

$(genParsers "worldCup" ["rosters", "Peru", "midfielders"])

$(genProviders "worldCup" ["rosters", "Peru", "midfielders"])

$(genParsers "worldCup" ["rosters", "Peru", "forwards"])

$(genProviders "worldCup" ["rosters", "Peru", "forwards"])

$(genParsers "worldCup" ["rosters", "Argentina", "coach"])

$(genProviders "worldCup" ["rosters", "Argentina", "coach"])

$(genParsers "worldCup" ["rosters", "Argentina", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Argentina", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Argentina", "defenders"])

$(genProviders "worldCup" ["rosters", "Argentina", "defenders"])

$(genParsers "worldCup" ["rosters", "Argentina", "midfielders"])

$(genProviders "worldCup" ["rosters", "Argentina", "midfielders"])

$(genParsers "worldCup" ["rosters", "Argentina", "forwards"])

$(genProviders "worldCup" ["rosters", "Argentina", "forwards"])

$(genParsers "worldCup" ["rosters", "Croatia", "coach"])

$(genProviders "worldCup" ["rosters", "Croatia", "coach"])

$(genParsers "worldCup" ["rosters", "Croatia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Croatia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Croatia", "defenders"])

$(genProviders "worldCup" ["rosters", "Croatia", "defenders"])

$(genParsers "worldCup" ["rosters", "Croatia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Croatia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Croatia", "forwards"])

$(genProviders "worldCup" ["rosters", "Croatia", "forwards"])

$(genParsers "worldCup" ["rosters", "Iceland", "coach"])

$(genProviders "worldCup" ["rosters", "Iceland", "coach"])

$(genParsers "worldCup" ["rosters", "Iceland", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Iceland", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Iceland", "defenders"])

$(genProviders "worldCup" ["rosters", "Iceland", "defenders"])

$(genParsers "worldCup" ["rosters", "Iceland", "midfielders"])

$(genProviders "worldCup" ["rosters", "Iceland", "midfielders"])

$(genParsers "worldCup" ["rosters", "Iceland", "forwards"])

$(genProviders "worldCup" ["rosters", "Iceland", "forwards"])

$(genParsers "worldCup" ["rosters", "Nigeria", "coach"])

$(genProviders "worldCup" ["rosters", "Nigeria", "coach"])

$(genParsers "worldCup" ["rosters", "Nigeria", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Nigeria", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Nigeria", "defenders"])

$(genProviders "worldCup" ["rosters", "Nigeria", "defenders"])

$(genParsers "worldCup" ["rosters", "Nigeria", "midfielders"])

$(genProviders "worldCup" ["rosters", "Nigeria", "midfielders"])

$(genParsers "worldCup" ["rosters", "Nigeria", "forwards"])

$(genProviders "worldCup" ["rosters", "Nigeria", "forwards"])

$(genParsers "worldCup" ["rosters", "Brazil", "coach"])

$(genProviders "worldCup" ["rosters", "Brazil", "coach"])

$(genParsers "worldCup" ["rosters", "Brazil", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Brazil", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Brazil", "defenders"])

$(genProviders "worldCup" ["rosters", "Brazil", "defenders"])

$(genParsers "worldCup" ["rosters", "Brazil", "midfielders"])

$(genProviders "worldCup" ["rosters", "Brazil", "midfielders"])

$(genParsers "worldCup" ["rosters", "Brazil", "forwards"])

$(genProviders "worldCup" ["rosters", "Brazil", "forwards"])

$(genParsers "worldCup" ["rosters", "Costa_Rica", "coach"])

$(genProviders "worldCup" ["rosters", "Costa_Rica", "coach"])

$(genParsers "worldCup" ["rosters", "Costa_Rica", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Costa_Rica", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Costa_Rica", "defenders"])

$(genProviders "worldCup" ["rosters", "Costa_Rica", "defenders"])

$(genParsers "worldCup" ["rosters", "Costa_Rica", "midfielders"])

$(genProviders "worldCup" ["rosters", "Costa_Rica", "midfielders"])

$(genParsers "worldCup" ["rosters", "Costa_Rica", "forwards"])

$(genProviders "worldCup" ["rosters", "Costa_Rica", "forwards"])

$(genParsers "worldCup" ["rosters", "Serbia", "coach"])

$(genProviders "worldCup" ["rosters", "Serbia", "coach"])

$(genParsers "worldCup" ["rosters", "Serbia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Serbia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Serbia", "defenders"])

$(genProviders "worldCup" ["rosters", "Serbia", "defenders"])

$(genParsers "worldCup" ["rosters", "Serbia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Serbia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Serbia", "forwards"])

$(genProviders "worldCup" ["rosters", "Serbia", "forwards"])

$(genParsers "worldCup" ["rosters", "Switzerland", "coach"])

$(genProviders "worldCup" ["rosters", "Switzerland", "coach"])

$(genParsers "worldCup" ["rosters", "Switzerland", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Switzerland", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Switzerland", "defenders"])

$(genProviders "worldCup" ["rosters", "Switzerland", "defenders"])

$(genParsers "worldCup" ["rosters", "Switzerland", "midfielders"])

$(genProviders "worldCup" ["rosters", "Switzerland", "midfielders"])

$(genParsers "worldCup" ["rosters", "Switzerland", "forwards"])

$(genProviders "worldCup" ["rosters", "Switzerland", "forwards"])

$(genParsers "worldCup" ["rosters", "Germany", "coach"])

$(genProviders "worldCup" ["rosters", "Germany", "coach"])

$(genParsers "worldCup" ["rosters", "Germany", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Germany", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Germany", "defenders"])

$(genProviders "worldCup" ["rosters", "Germany", "defenders"])

$(genParsers "worldCup" ["rosters", "Germany", "midfielders"])

$(genProviders "worldCup" ["rosters", "Germany", "midfielders"])

$(genParsers "worldCup" ["rosters", "Germany", "forwards"])

$(genProviders "worldCup" ["rosters", "Germany", "forwards"])

$(genParsers "worldCup" ["rosters", "Mexico", "coach"])

$(genProviders "worldCup" ["rosters", "Mexico", "coach"])

$(genParsers "worldCup" ["rosters", "Mexico", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Mexico", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Mexico", "defenders"])

$(genProviders "worldCup" ["rosters", "Mexico", "defenders"])

$(genParsers "worldCup" ["rosters", "Mexico", "midfielders"])

$(genProviders "worldCup" ["rosters", "Mexico", "midfielders"])

$(genParsers "worldCup" ["rosters", "Mexico", "forwards"])

$(genProviders "worldCup" ["rosters", "Mexico", "forwards"])

$(genParsers "worldCup" ["rosters", "South Korea", "coach"])

$(genProviders "worldCup" ["rosters", "South Korea", "coach"])

$(genParsers "worldCup" ["rosters", "South Korea", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "South Korea", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "South Korea", "defenders"])

$(genProviders "worldCup" ["rosters", "South Korea", "defenders"])

$(genParsers "worldCup" ["rosters", "South Korea", "midfielders"])

$(genProviders "worldCup" ["rosters", "South Korea", "midfielders"])

$(genParsers "worldCup" ["rosters", "South Korea", "forwards"])

$(genProviders "worldCup" ["rosters", "South Korea", "forwards"])

$(genParsers "worldCup" ["rosters", "Sweden", "coach"])

$(genProviders "worldCup" ["rosters", "Sweden", "coach"])

$(genParsers "worldCup" ["rosters", "Sweden", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Sweden", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Sweden", "defenders"])

$(genProviders "worldCup" ["rosters", "Sweden", "defenders"])

$(genParsers "worldCup" ["rosters", "Sweden", "midfielders"])

$(genProviders "worldCup" ["rosters", "Sweden", "midfielders"])

$(genParsers "worldCup" ["rosters", "Sweden", "forwards"])

$(genProviders "worldCup" ["rosters", "Sweden", "forwards"])

$(genParsers "worldCup" ["rosters", "Belgium", "coach"])

$(genProviders "worldCup" ["rosters", "Belgium", "coach"])

$(genParsers "worldCup" ["rosters", "Belgium", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Belgium", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Belgium", "defenders"])

$(genProviders "worldCup" ["rosters", "Belgium", "defenders"])

$(genParsers "worldCup" ["rosters", "Belgium", "midfielders"])

$(genProviders "worldCup" ["rosters", "Belgium", "midfielders"])

$(genParsers "worldCup" ["rosters", "Belgium", "forwards"])

$(genProviders "worldCup" ["rosters", "Belgium", "forwards"])

$(genParsers "worldCup" ["rosters", "England", "coach"])

$(genProviders "worldCup" ["rosters", "England", "coach"])

$(genParsers "worldCup" ["rosters", "England", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "England", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "England", "defenders"])

$(genProviders "worldCup" ["rosters", "England", "defenders"])

$(genParsers "worldCup" ["rosters", "England", "midfielders"])

$(genProviders "worldCup" ["rosters", "England", "midfielders"])

$(genParsers "worldCup" ["rosters", "England", "forwards"])

$(genProviders "worldCup" ["rosters", "England", "forwards"])

$(genParsers "worldCup" ["rosters", "Panama", "coach"])

$(genProviders "worldCup" ["rosters", "Panama", "coach"])

$(genParsers "worldCup" ["rosters", "Panama", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Panama", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Panama", "defenders"])

$(genProviders "worldCup" ["rosters", "Panama", "defenders"])

$(genParsers "worldCup" ["rosters", "Panama", "midfielders"])

$(genProviders "worldCup" ["rosters", "Panama", "midfielders"])

$(genParsers "worldCup" ["rosters", "Panama", "forwards"])

$(genProviders "worldCup" ["rosters", "Panama", "forwards"])

$(genParsers "worldCup" ["rosters", "Tunisia", "coach"])

$(genProviders "worldCup" ["rosters", "Tunisia", "coach"])

$(genParsers "worldCup" ["rosters", "Tunisia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Tunisia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Tunisia", "defenders"])

$(genProviders "worldCup" ["rosters", "Tunisia", "defenders"])

$(genParsers "worldCup" ["rosters", "Tunisia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Tunisia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Tunisia", "forwards"])

$(genProviders "worldCup" ["rosters", "Tunisia", "forwards"])

$(genParsers "worldCup" ["rosters", "Columbia", "coach"])

$(genProviders "worldCup" ["rosters", "Columbia", "coach"])

$(genParsers "worldCup" ["rosters", "Columbia", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Columbia", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Columbia", "defenders"])

$(genProviders "worldCup" ["rosters", "Columbia", "defenders"])

$(genParsers "worldCup" ["rosters", "Columbia", "midfielders"])

$(genProviders "worldCup" ["rosters", "Columbia", "midfielders"])

$(genParsers "worldCup" ["rosters", "Columbia", "forwards"])

$(genProviders "worldCup" ["rosters", "Columbia", "forwards"])

$(genParsers "worldCup" ["rosters", "Japan", "coach"])

$(genProviders "worldCup" ["rosters", "Japan", "coach"])

$(genParsers "worldCup" ["rosters", "Japan", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Japan", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Japan", "defenders"])

$(genProviders "worldCup" ["rosters", "Japan", "defenders"])

$(genParsers "worldCup" ["rosters", "Japan", "midfielders"])

$(genProviders "worldCup" ["rosters", "Japan", "midfielders"])

$(genParsers "worldCup" ["rosters", "Japan", "forwards"])

$(genProviders "worldCup" ["rosters", "Japan", "forwards"])

$(genParsers "worldCup" ["rosters", "Poland", "coach"])

$(genProviders "worldCup" ["rosters", "Poland", "coach"])

$(genParsers "worldCup" ["rosters", "Poland", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Poland", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Poland", "defenders"])

$(genProviders "worldCup" ["rosters", "Poland", "defenders"])

$(genParsers "worldCup" ["rosters", "Poland", "midfielders"])

$(genProviders "worldCup" ["rosters", "Poland", "midfielders"])

$(genParsers "worldCup" ["rosters", "Poland", "forwards"])

$(genProviders "worldCup" ["rosters", "Poland", "forwards"])

$(genParsers "worldCup" ["rosters", "Senegal", "coach"])

$(genProviders "worldCup" ["rosters", "Senegal", "coach"])

$(genParsers "worldCup" ["rosters", "Senegal", "goalkeepers"])

$(genProviders "worldCup" ["rosters", "Senegal", "goalkeepers"])

$(genParsers "worldCup" ["rosters", "Senegal", "defenders"])

$(genProviders "worldCup" ["rosters", "Senegal", "defenders"])

$(genParsers "worldCup" ["rosters", "Senegal", "midfielders"])

$(genProviders "worldCup" ["rosters", "Senegal", "midfielders"])

$(genParsers "worldCup" ["rosters", "Senegal", "forwards"])

$(genProviders "worldCup" ["rosters", "Senegal", "forwards"])
