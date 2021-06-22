{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.HowToTrainYourDragon where

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

parseHowToTrainYourDragon :: FromJSON a => FakerSettings -> Value -> Parser a
parseHowToTrainYourDragon settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  howToTrainYourDragon <- faker .: "how_to_train_your_dragon"
  pure howToTrainYourDragon
parseHowToTrainYourDragon settings val = fail $ "expected Object, but got " <> (show val)

parseHowToTrainYourDragonField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseHowToTrainYourDragonField settings txt val = do
  howToTrainYourDragon <- parseHowToTrainYourDragon settings val
  field <- howToTrainYourDragon .:? txt .!= mempty
  pure field

parseHowToTrainYourDragonFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseHowToTrainYourDragonFields settings txts val = do
  howToTrainYourDragon <- parseHowToTrainYourDragon settings val
  helper howToTrainYourDragon txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedHowToTrainYourDragonFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedHowToTrainYourDragonFields settings txts val = do
  howToTrainYourDragon <- parseHowToTrainYourDragon settings val
  helper howToTrainYourDragon txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "howToTrainYourDragon" "characters")

$(genProvider "howToTrainYourDragon" "characters")

$(genParser "howToTrainYourDragon" "dragons")

$(genProvider "howToTrainYourDragon" "dragons")

$(genParser "howToTrainYourDragon" "locations")

$(genProvider "howToTrainYourDragon" "locations")








