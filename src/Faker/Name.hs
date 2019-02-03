module Faker.Name where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Name

maleFirstName :: Fake Text
maleFirstName = Fake (\settings -> randomVec settings maleFirstNameProvider)

femaleFirstName :: Fake Text
femaleFirstName = Fake (\settings -> randomVec settings femaleFirstNameProvider)

prefix :: Fake Text
prefix = Fake (\settings -> randomVec settings prefixProvider)

suffix :: Fake Text
suffix = Fake (\settings -> randomVec settings suffixProvider)

name :: Fake Text
name = Fake (\settings -> randomUnresolvedVec settings nameProvider resolveNameText)

nameWithMiddle :: Fake Text
nameWithMiddle = Fake (\settings -> randomUnresolvedVec settings nameWithMiddleProvider resolveNameText)
