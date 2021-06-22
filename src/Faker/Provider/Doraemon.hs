{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Doraemon where

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

parseDoraemon :: FromJSON a => FakerSettings -> Value -> Parser a
parseDoraemon settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  doraemon <- faker .: "doraemon"
  pure doraemon
parseDoraemon settings val = fail $ "expected Object, but got " <> (show val)

parseDoraemonField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDoraemonField settings txt val = do
  doraemon <- parseDoraemon settings val
  field <- doraemon .:? txt .!= mempty
  pure field

parseDoraemonFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseDoraemonFields settings txts val = do
  doraemon <- parseDoraemon settings val
  helper doraemon txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedDoraemonFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedDoraemonFields settings txts val = do
  doraemon <- parseDoraemon settings val
  helper doraemon txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "doraemon" "characters")

$(genProvider "doraemon" "characters")

$(genParser "doraemon" "gadgets")

$(genProvider "doraemon" "gadgets")

$(genParser "doraemon" "locations")

$(genProvider "doraemon" "locations")








