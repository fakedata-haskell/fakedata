{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Bird where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseBird :: FromJSON a => FakerSettings -> Value -> Parser a
parseBird settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  creature <- faker .: "creature"
  bird <- creature .: "bird"
  pure bird
parseBird settings val = fail $ "expected Object, but got " <> (show val)

parseBirdField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBirdField settings txt val = do
  bird <- parseBird settings val
  field <- bird .:? txt .!= mempty
  pure field

parseBirdFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseBirdFields settings txts val = do
  bird <- parseBird settings val
  helper bird txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)


parseUnresolvedBirdField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBirdField settings txt val = do
  bird <- parseBird settings val
  field <- bird .:? txt .!= mempty
  pure $ pure field



parseUnresolvedBirdFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBirdFields settings txts val = do
  bird <- parseBird settings val
  helper bird txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "bird" "anatomy")

$(genProvider "bird" "anatomy")

$(genParser "bird" "anatomy_past_tense")

$(genProvider "bird" "anatomy_past_tense")

$(genParser "bird" "geo")

$(genProvider "bird" "geo")

$(genParser "bird" "colors")

$(genProvider "bird" "colors")

$(genParser "bird" "emotional_adjectives")

$(genProvider "bird" "emotional_adjectives")

$(genParser "bird" "silly_adjectives")

$(genProvider "bird" "silly_adjectives")

$(genParser "bird" "adjectives")

$(genProvider "bird" "adjectives")

$(genParser "bird" "common_family_name")

$(genProvider "bird" "common_family_name")



$(genParserUnresolved "bird" "plausible_common_names")
$(genProviderUnresolved "bird" "plausible_common_names")
$(genParserUnresolved "bird" "implausible_common_names")
$(genProviderUnresolved "bird" "implausible_common_names")


resolveBirdText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveBirdText = genericResolver' resolveBirdField

resolveBirdField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveBirdField settings field@"geo" =
  cachedRandomVec "bird" field birdGeoProvider settings
resolveBirdField settings str = throwM $ InvalidField "bird" str


$(genParsers "bird" ["order_common_map","Accipitriformes"])
$(genProviders "bird" ["order_common_map","Accipitriformes"])

$(genParsers "bird" ["order_common_map","Anseriformes"])
$(genProviders "bird" ["order_common_map","Anseriformes"])

$(genParsers "bird" ["order_common_map","Apterygiformes"])
$(genProviders "bird" ["order_common_map","Apterygiformes"])

$(genParsers "bird" ["order_common_map","Bucerotiformes"])
$(genProviders "bird" ["order_common_map","Bucerotiformes"])

$(genParsers "bird" ["order_common_map","Caprimulgiformes"])
$(genProviders "bird" ["order_common_map","Caprimulgiformes"])

$(genParsers "bird" ["order_common_map","Cariamiformes"])
$(genProviders "bird" ["order_common_map","Cariamiformes"])

$(genParsers "bird" ["order_common_map","Casuariiformes"])
$(genProviders "bird" ["order_common_map","Casuariiformes"])

$(genParsers "bird" ["order_common_map","Cathartiformes"])
$(genProviders "bird" ["order_common_map","Cathartiformes"])

$(genParsers "bird" ["order_common_map","Charadriiformes"])
$(genProviders "bird" ["order_common_map","Charadriiformes"])

$(genParsers "bird" ["order_common_map","Ciconiiformes"])
$(genProviders "bird" ["order_common_map","Ciconiiformes"])

$(genParsers "bird" ["order_common_map","Coliiformes"])
$(genProviders "bird" ["order_common_map","Coliiformes"])

$(genParsers "bird" ["order_common_map","Columbiformes"])
$(genProviders "bird" ["order_common_map","Columbiformes"])

$(genParsers "bird" ["order_common_map","Coraciiformes"])
$(genProviders "bird" ["order_common_map","Coraciiformes"])

$(genParsers "bird" ["order_common_map","Cuculiformes"])
$(genProviders "bird" ["order_common_map","Cuculiformes"])

$(genParsers "bird" ["order_common_map","Eurypygiformes"])
$(genProviders "bird" ["order_common_map","Eurypygiformes"])

$(genParsers "bird" ["order_common_map","Falconiformes"])
$(genProviders "bird" ["order_common_map","Falconiformes"])

$(genParsers "bird" ["order_common_map","Galbuliformes"])
$(genProviders "bird" ["order_common_map","Galbuliformes"])

$(genParsers "bird" ["order_common_map","Galliformes"])
$(genProviders "bird" ["order_common_map","Galliformes"])

$(genParsers "bird" ["order_common_map","Gaviiformes"])
$(genProviders "bird" ["order_common_map","Gaviiformes"])

$(genParsers "bird" ["order_common_map","Gruiformes"])
$(genProviders "bird" ["order_common_map","Gruiformes"])

$(genParsers "bird" ["order_common_map","Mesitornithiformes"])
$(genProviders "bird" ["order_common_map","Mesitornithiformes"])

$(genParsers "bird" ["order_common_map","Musophagiformes"])
$(genProviders "bird" ["order_common_map","Musophagiformes"])

$(genParsers "bird" ["order_common_map","Opisthocomiformes"])
$(genProviders "bird" ["order_common_map","Opisthocomiformes"])

$(genParsers "bird" ["order_common_map","Otidiformes"])
$(genProviders "bird" ["order_common_map","Otidiformes"])

$(genParsers "bird" ["order_common_map","Passeriformes"])
$(genProviders "bird" ["order_common_map","Passeriformes"])

$(genParsers "bird" ["order_common_map","Pelecaniformes"])
$(genProviders "bird" ["order_common_map","Pelecaniformes"])

$(genParsers "bird" ["order_common_map","Phaethontiformes"])
$(genProviders "bird" ["order_common_map","Phaethontiformes"])

$(genParsers "bird" ["order_common_map","Phoenicopteriformes"])
$(genProviders "bird" ["order_common_map","Phoenicopteriformes"])

$(genParsers "bird" ["order_common_map","Piciformes"])
$(genProviders "bird" ["order_common_map","Piciformes"])

$(genParsers "bird" ["order_common_map","Podicipediformes"])
$(genProviders "bird" ["order_common_map","Podicipediformes"])

$(genParsers "bird" ["order_common_map","Procellariiformes"])
$(genProviders "bird" ["order_common_map","Procellariiformes"])

$(genParsers "bird" ["order_common_map","Psittaciformes"])
$(genProviders "bird" ["order_common_map","Psittaciformes"])

$(genParsers "bird" ["order_common_map","Pterocliformes"])
$(genProviders "bird" ["order_common_map","Pterocliformes"])

$(genParsers "bird" ["order_common_map","Rheiformes"])
$(genProviders "bird" ["order_common_map","Rheiformes"])

$(genParsers "bird" ["order_common_map","Sphenisciformes"])
$(genProviders "bird" ["order_common_map","Sphenisciformes"])

$(genParsers "bird" ["order_common_map","Strigiformes"])
$(genProviders "bird" ["order_common_map","Strigiformes"])

$(genParsers "bird" ["order_common_map","Struthioniformes"])
$(genProviders "bird" ["order_common_map","Struthioniformes"])

$(genParsers "bird" ["order_common_map","Suliformes"])
$(genProviders "bird" ["order_common_map","Suliformes"])

$(genParsers "bird" ["order_common_map","Tinamiformes"])
$(genProviders "bird" ["order_common_map","Tinamiformes"])

$(genParsers "bird" ["order_common_map","Trogoniformes"])
$(genProviders "bird" ["order_common_map","Trogoniformes"])
