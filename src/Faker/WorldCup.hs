{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.WorldCup where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.WorldCup
import Faker.TH

$(generateFakeField "worldCup" "teams")

$(generateFakeField "worldCup" "stadiums")

$(generateFakeField "worldCup" "cities")

$(generateFakeFields "worldCup" ["groups", "group_A"])

$(generateFakeFields "worldCup" ["groups", "group_B"])

$(generateFakeFields "worldCup" ["groups", "group_C"])

$(generateFakeFields "worldCup" ["groups", "group_D"])

$(generateFakeFields "worldCup" ["groups", "group_E"])

$(generateFakeFields "worldCup" ["groups", "group_F"])

$(generateFakeFields "worldCup" ["groups", "group_G"])

$(generateFakeFields "worldCup" ["groups", "group_H"])

$(generateFakeFields "worldCup" ["rosters", "Egypt", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Egypt", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Egypt", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Egypt", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Egypt", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Russia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Russia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Russia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Russia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Russia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Saudi_Arabia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Saudi_Arabia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Saudi_Arabia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Saudi_Arabia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Saudi_Arabia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Uruguay", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Uruguay", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Uruguay", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Uruguay", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Uruguay", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Iran", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Iran", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Iran", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Iran", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Iran", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Morocco", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Morocco", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Morocco", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Morocco", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Morocco", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Portugal", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Portugal", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Portugal", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Portugal", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Portugal", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Spain", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Spain", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Spain", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Spain", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Spain", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Australia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Australia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Australia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Australia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Australia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Denmark", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Denmark", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Denmark", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Denmark", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Denmark", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "France", "coach"])

$(generateFakeFields "worldCup" ["rosters", "France", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "France", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "France", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "France", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Peru", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Peru", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Peru", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Peru", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Peru", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Argentina", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Argentina", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Argentina", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Argentina", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Argentina", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Croatia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Croatia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Croatia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Croatia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Croatia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Iceland", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Iceland", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Iceland", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Iceland", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Iceland", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Nigeria", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Nigeria", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Nigeria", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Nigeria", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Nigeria", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Brazil", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Brazil", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Brazil", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Brazil", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Brazil", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Costa_Rica", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Costa_Rica", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Costa_Rica", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Costa_Rica", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Costa_Rica", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Serbia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Serbia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Serbia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Serbia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Serbia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Switzerland", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Switzerland", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Switzerland", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Switzerland", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Switzerland", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Germany", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Germany", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Germany", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Germany", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Germany", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Mexico", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Mexico", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Mexico", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Mexico", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Mexico", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "South Korea", "coach"])

$(generateFakeFields "worldCup" ["rosters", "South Korea", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "South Korea", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "South Korea", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "South Korea", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Sweden", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Sweden", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Sweden", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Sweden", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Sweden", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Belgium", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Belgium", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Belgium", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Belgium", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Belgium", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "England", "coach"])

$(generateFakeFields "worldCup" ["rosters", "England", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "England", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "England", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "England", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Panama", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Panama", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Panama", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Panama", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Panama", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Tunisia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Tunisia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Tunisia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Tunisia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Tunisia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Columbia", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Columbia", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Columbia", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Columbia", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Columbia", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Japan", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Japan", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Japan", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Japan", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Japan", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Poland", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Poland", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Poland", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Poland", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Poland", "forwards"])

$(generateFakeFields "worldCup" ["rosters", "Senegal", "coach"])

$(generateFakeFields "worldCup" ["rosters", "Senegal", "goalkeepers"])

$(generateFakeFields "worldCup" ["rosters", "Senegal", "defenders"])

$(generateFakeFields "worldCup" ["rosters", "Senegal", "midfielders"])

$(generateFakeFields "worldCup" ["rosters", "Senegal", "forwards"])
