module Faker.BackToTheFuture where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.BackToTheFuture

character :: Fake Text
character = Fake (\settings -> randomVec settings bttfCharacterProvider)

date :: Fake Text
date = Fake (\settings -> randomVec settings bttfDatesProvider)

quote :: Fake Text
quote = Fake (\settings -> randomVec settings bttfQuoteProvider)
