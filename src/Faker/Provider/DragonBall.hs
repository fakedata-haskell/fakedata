{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.DragonBall where

import Config
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

parseDragonBall :: FromJSON a => FakerSettings -> Value -> Parser a
parseDragonBall settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  dragonBall <- faker .: "dragon_ball"
  pure dragonBall
parseDragonBall settings val = fail $ "expected Object, but got " <> (show val)

parseDragonBallField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDragonBallField settings txt val = do
  dragonBall <- parseDragonBall settings val
  field <- dragonBall .:? txt .!= mempty
  pure field

$(genParser "dragonBall" "characters")

$(genProvider "dragonBall" "characters")

$(genParser "dragonBall" "races")

$(genProvider "dragonBall" "races")

$(genParser "dragonBall" "planets")

$(genProvider "dragonBall" "planets")
