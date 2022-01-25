{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Opera where

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
import qualified Data.Aeson.Key as K

parseOpera :: FromJSON a => FakerSettings -> Value -> Parser a
parseOpera settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  opera <- faker .: "opera"
  pure opera
parseOpera settings val = fail $ "expected Object, but got " <> (show val)

parseOperaField ::
     (FromJSON a, Monoid a) => FakerSettings -> K.Key -> Value -> Parser a
parseOperaField settings txt val = do
  opera <- parseOpera settings val
  field <- opera .:? txt .!= mempty
  pure field

parseOperaFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [K.Key] -> Value -> Parser a
parseOperaFields settings txts val = do
  opera <- parseOpera settings val
  helper opera txts
  where
    helper :: (FromJSON a) => Value -> [K.Key] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParsers "opera" ["italian", "by_giuseppe_verdi"])

$(genProviders "opera" ["italian", "by_giuseppe_verdi"])

$(genParsers "opera" ["italian", "by_gioacchino_rossini"])

$(genProviders "opera" ["italian", "by_gioacchino_rossini"])

$(genParsers "opera" ["italian", "by_gaetano_donizetti"])

$(genProviders "opera" ["italian", "by_gaetano_donizetti"])

$(genParsers "opera" ["italian", "by_vincenzo_bellini"])

$(genProviders "opera" ["italian", "by_vincenzo_bellini"])

$(genParsers "opera" ["italian", "by_christoph_willibald_gluck"])

$(genProviders "opera" ["italian", "by_christoph_willibald_gluck"])

$(genParsers "opera" ["italian", "by_wolfgang_amadeus_mozart"])

$(genProviders "opera" ["italian", "by_wolfgang_amadeus_mozart"])

$(genParsers "opera" ["german", "by_wolfgang_amadeus_mozart"])

$(genProviders "opera" ["german", "by_wolfgang_amadeus_mozart"])

$(genParsers "opera" ["german", "by_ludwig_van_beethoven"])

$(genProviders "opera" ["german", "by_ludwig_van_beethoven"])

$(genParsers "opera" ["german", "by_carl_maria_von_weber"])

$(genProviders "opera" ["german", "by_carl_maria_von_weber"])

$(genParsers "opera" ["german", "by_richard_strauss"])

$(genProviders "opera" ["german", "by_richard_strauss"])

$(genParsers "opera" ["german", "by_richard_wagner"])

$(genProviders "opera" ["german", "by_richard_wagner"])

$(genParsers "opera" ["german", "by_robert_schumann"])

$(genProviders "opera" ["german", "by_robert_schumann"])

$(genParsers "opera" ["german", "by_franz_schubert"])

$(genProviders "opera" ["german", "by_franz_schubert"])

$(genParsers "opera" ["german", "by_alban_berg"])

$(genProviders "opera" ["german", "by_alban_berg"])

$(genParsers "opera" ["french", "by_christoph_willibald_gluck"])

$(genProviders "opera" ["french", "by_christoph_willibald_gluck"])

$(genParsers "opera" ["french", "by_maurice_ravel"])

$(genProviders "opera" ["french", "by_maurice_ravel"])

$(genParsers "opera" ["french", "by_hector_berlioz"])

$(genProviders "opera" ["french", "by_hector_berlioz"])

$(genParsers "opera" ["french", "by_georges_bizet"])

$(genProviders "opera" ["french", "by_georges_bizet"])

$(genParsers "opera" ["french", "by_charles_gounod"])

$(genProviders "opera" ["french", "by_charles_gounod"])

$(genParsers "opera" ["french", "by_camille_saint_saëns"])

$(genProviders "opera" ["french", "by_camille_saint_saëns"])
