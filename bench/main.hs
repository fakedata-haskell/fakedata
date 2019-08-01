{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.DeepSeq
import Data.Text (Text)
import Faker
import Faker.Address (country, fullAddress)
import Faker.Ancient (god)
import Faker.Combinators (listOf)
import Faker.Name (name)
import Faker.TvShow.SiliconValley (email)
import GHC.Generics
import Gauge

data Person =
  Person
    { personName :: Text
    , personAddress :: Text
    , personEmail :: Text
    , personCountry :: Text
    }
  deriving (Show, Eq, NFData, Generic)

fakePerson :: Fake Person
fakePerson = do
  personName <- name
  personAddress <- fullAddress
  personCountry <- country
  personEmail <- email
  pure $ Person {..}

main :: IO ()
main = defaultMain benchs
  where
    benchs =
      [ bgroup
          "God benchmark"
          [ bench "single god" $ nfIO (generate singleGod)
          , bench "thousand gods" $ nfIO (generate thousandGod)
          ]
      -- , bgroup
      --     "Person benchmark"
      --     [ bench "single person" $ nfIO (generate fakePerson)
      --     , bench "10 persons" $ nfIO (generate $ listOf 10 fakePerson)
      --     ]
      ]
      where
        singleGod :: Fake Text
        singleGod = god
        thousandGod :: Fake [Text]
        thousandGod = listOf 1000 god
