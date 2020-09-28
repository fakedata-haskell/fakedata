{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.StudioGhibli where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseStudioGhibli :: FromJSON a => FakerSettings -> Value -> Parser a
parseStudioGhibli settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  studioGhibli <- faker .: "studio_ghibli"
  pure studioGhibli
parseStudioGhibli settings val = fail $ "expected Object, but got " <> (show val)

parseStudioGhibliField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseStudioGhibliField settings txt val = do
  studioGhibli <- parseStudioGhibli settings val
  field <- studioGhibli .:? txt .!= mempty
  pure field

parseStudioGhibliFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseStudioGhibliFields settings txts val = do
  studioGhibli <- parseStudioGhibli settings val
  helper studioGhibli txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedStudioGhibliFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedStudioGhibliFields settings txts val = do
  studioGhibli <- parseStudioGhibli settings val
  helper studioGhibli txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "studioGhibli" "characters")

$(genProvider "studioGhibli" "characters")


$(genParser "studioGhibli" "quotes")

$(genProvider "studioGhibli" "quotes")


$(genParser "studioGhibli" "movies")

$(genProvider "studioGhibli" "movies")











