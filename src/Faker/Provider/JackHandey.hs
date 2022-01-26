{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.JackHandey where

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


parseJackHandey :: FromJSON a => FakerSettings -> Value -> Parser a
parseJackHandey settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  jackHandey <- faker .: "quote"
  pure jackHandey
parseJackHandey settings val = fail $ "expected Object, but got " <> (show val)

parseJackHandeyField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseJackHandeyField settings txt val = do
  jackHandey <- parseJackHandey settings val
  field <- jackHandey .:? txt .!= mempty
  pure field

parseJackHandeyFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseJackHandeyFields settings txts val = do
  jackHandey <- parseJackHandey settings val
  helper jackHandey txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedJackHandeyFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedJackHandeyFields settings txts val = do
  jackHandey <- parseJackHandey settings val
  helper jackHandey txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "jackHandey" "jack_handey")

$(genProvider "jackHandey" "jack_handey")
