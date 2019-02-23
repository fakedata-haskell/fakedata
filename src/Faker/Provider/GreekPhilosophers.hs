{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.GreekPhilosophers where

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

parseGreekPhilosophers :: FromJSON a => FakerSettings -> Value -> Parser a
parseGreekPhilosophers settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  greekPhilosophers <- faker .: "greek_philosophers"
  pure greekPhilosophers
parseGreekPhilosophers settings val = fail $ "expected Object, but got " <> (show val)

parseGreekPhilosophersField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseGreekPhilosophersField settings txt val = do
  greekPhilosophers <- parseGreekPhilosophers settings val
  field <- greekPhilosophers .:? txt .!= mempty
  pure field

parseGreekPhilosophersFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseGreekPhilosophersFields settings txts val = do
  greekPhilosophers <- parseGreekPhilosophers settings val
  helper greekPhilosophers txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




$(genParser "greekPhilosophers" "names")

$(genProvider "greekPhilosophers" "names")


$(genParser "greekPhilosophers" "quotes")

$(genProvider "greekPhilosophers" "quotes")











