{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Superhero where

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

parseSuperhero :: FromJSON a => FakerSettings -> Value -> Parser a
parseSuperhero settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  superhero <- faker .: "superhero"
  pure superhero
parseSuperhero settings val = fail $ "expected Object, but got " <> (show val)

parseSuperheroField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseSuperheroField settings txt val = do
  superhero <- parseSuperhero settings val
  field <- superhero .:? txt .!= mempty
  pure field

parseSuperheroFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseSuperheroFields settings txts val = do
  superhero <- parseSuperhero settings val
  helper superhero txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedSuperheroField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedSuperheroField settings txt val = do
  superhero <- parseSuperhero settings val
  field <- superhero .:? txt .!= mempty
  pure $ pure field

$(genParser "superhero" "power")

$(genProvider "superhero" "power")

$(genParser "superhero" "prefix")

$(genProvider "superhero" "prefix")

$(genParser "superhero" "suffix")

$(genProvider "superhero" "suffix")

$(genParser "superhero" "descriptor")

$(genProvider "superhero" "descriptor")

$(genParserUnresolved "superhero" "name")

$(genProviderUnresolved "superhero" "name")

resolveSuperheroText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveSuperheroText settings txt = do
  let fields = resolveFields txt
  superheroFields <- mapM (resolveSuperheroField settings) fields
  pure $ operateFields txt superheroFields

resolveSuperheroField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveSuperheroField settings "Superhero.prefix" =
  randomVec settings superheroPrefixProvider
resolveSuperheroField settings "Superhero.suffix" =
  randomVec settings superheroSuffixProvider
resolveSuperheroField settings "Superhero.descriptor" =
  randomVec settings superheroDescriptorProvider
resolveSuperheroField settings str = throwM $ InvalidField "superhero" str
