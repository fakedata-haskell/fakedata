{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Music where

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

parseMusic :: FromJSON a => FakerSettings -> Value -> Parser a
parseMusic settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  music <- faker .: "music"
  pure music
parseMusic settings val = fail $ "expected Object, but got " <> (show val)

parseMusicField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMusicField settings txt val = do
  music <- parseMusic settings val
  field <- music .:? txt .!= mempty
  pure field

parseMusicFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMusicFields settings txts val = do
  music <- parseMusic settings val
  helper music txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedMusicFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedMusicFields settings txts val = do
  music <- parseMusic settings val
  helper music txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "music" "instruments")

$(genProvider "music" "instruments")

$(genParser "music" "bands")

$(genProvider "music" "bands")

$(genParser "music" "albums")

$(genProvider "music" "albums")

$(genParser "music" "genres")

$(genProvider "music" "genres")

$(genParser "music" "mambo_no_5")

$(genProvider "music" "mambo_no_5")







$(genParsers "music" ["hiphop","subgenres"])
$(genProviders "music" ["hiphop","subgenres"])

$(genParsers "music" ["hiphop","groups"])
$(genProviders "music" ["hiphop","groups"])

$(genParsers "music" ["hiphop","artist"])
$(genProviders "music" ["hiphop","artist"])


