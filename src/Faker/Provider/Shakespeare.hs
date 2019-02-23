{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Shakespeare where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseShakespeare :: FromJSON a => FakerSettings -> Value -> Parser a
parseShakespeare settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  shakespeare <- faker .: "shakespeare"
  pure shakespeare
parseShakespeare settings val = fail $ "expected Object, but got " <> (show val)

parseShakespeareField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseShakespeareField settings txt val = do
  shakespeare <- parseShakespeare settings val
  field <- shakespeare .:? txt .!= mempty
  pure field

parseShakespeareFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseShakespeareFields settings txts val = do
  shakespeare <- parseShakespeare settings val
  helper shakespeare txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "shakespeare" "hamlet")

$(genProvider "shakespeare" "hamlet")


$(genParser "shakespeare" "as_you_like_it")

$(genProvider "shakespeare" "as_you_like_it")


$(genParser "shakespeare" "king_richard_iii")

$(genProvider "shakespeare" "king_richard_iii")


$(genParser "shakespeare" "romeo_and_juliet")

$(genProvider "shakespeare" "romeo_and_juliet")











