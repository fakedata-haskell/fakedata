module Faker.TvShow.BoJackHorseman where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BoJackHorseman

character :: Fake Text
character = Fake $ resolver boJackHorsemanCharacterProvider

quote :: Fake Text
quote = Fake $ resolver boJackHorsemanQuoteProvider

tongueTwister :: Fake Text
tongueTwister = Fake $ resolver boJackHorsemanTongueTwisterProvider
