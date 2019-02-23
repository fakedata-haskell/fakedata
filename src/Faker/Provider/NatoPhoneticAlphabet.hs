{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.NatoPhoneticAlphabet where

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

parseNatoPhoneticAlphabet :: FromJSON a => FakerSettings -> Value -> Parser a
parseNatoPhoneticAlphabet settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  natoPhoneticAlphabet <- faker .: "nato_phonetic_alphabet"
  pure natoPhoneticAlphabet
parseNatoPhoneticAlphabet settings val = fail $ "expected Object, but got " <> (show val)

parseNatoPhoneticAlphabetField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseNatoPhoneticAlphabetField settings txt val = do
  natoPhoneticAlphabet <- parseNatoPhoneticAlphabet settings val
  field <- natoPhoneticAlphabet .:? txt .!= mempty
  pure field

parseNatoPhoneticAlphabetFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseNatoPhoneticAlphabetFields settings txts val = do
  natoPhoneticAlphabet <- parseNatoPhoneticAlphabet settings val
  helper natoPhoneticAlphabet txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "natoPhoneticAlphabet" "code_word")

$(genProvider "natoPhoneticAlphabet" "code_word")











