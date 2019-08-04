{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.SlackEmoji where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.SlackEmoji
import Faker.TH

$(generateFakeField "slackEmoji" "people")

$(generateFakeField "slackEmoji" "nature")

$(generateFakeField "slackEmoji" "food_and_drink")

$(generateFakeField "slackEmoji" "celebration")

$(generateFakeField "slackEmoji" "activity")

$(generateFakeField "slackEmoji" "travel_and_places")

$(generateFakeField "slackEmoji" "objects_and_symbols")

$(generateFakeField "slackEmoji" "custom")

$(generateFakeFieldUnresolved "slackEmoji" "emoji")
