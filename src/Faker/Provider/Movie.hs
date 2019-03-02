{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Movie where

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

parseMovie :: FromJSON a => FakerSettings -> Value -> Parser a
parseMovie settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  movie <- faker .: "movie"
  pure movie
parseMovie settings val = fail $ "expected Object, but got " <> (show val)

parseMovieField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMovieField settings txt val = do
  movie <- parseMovie settings val
  field <- movie .:? txt .!= mempty
  pure field

parseMovieFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMovieFields settings txts val = do
  movie <- parseMovie settings val
  helper movie txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "movie" "quote")

$(genProvider "movie" "quote")
