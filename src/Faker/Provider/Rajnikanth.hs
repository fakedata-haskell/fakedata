{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Rajnikanth where

import Config
import Control.Monad.Catch
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseRajnikanth :: FromJSON a => FakerSettings -> Value -> Parser a
parseRajnikanth settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  rajnikanth <- faker .: "rajnikanth"
  pure rajnikanth
parseRajnikanth settings val = fail $ "expected Object, but got " <> (show val)

parseRajnikanthField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseRajnikanthField settings txt val = do
  rajnikanth <- parseRajnikanth settings val
  field <- rajnikanth .:? txt .!= mempty
  pure field

parseRajnikanthFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseRajnikanthFields settings txts val = do
  rajnikanth <- parseRajnikanth settings val
  helper rajnikanth txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "rajnikanth" "joke")

$(genProvider "rajnikanth" "joke")
