{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Superhero where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseSuperhero :: FromJSON a => FakerSettings -> Value -> Parser a
parseSuperhero settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  superhero <- faker .: "superhero"
  pure superhero
parseSuperhero settings val = fail $ "expected Object, but got " <> (show val)

parseSuperheroField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseSuperheroField settings txt val = do
  superhero <- parseSuperhero settings val
  field <- superhero .:? txt .!= mempty
  pure field

parseSuperheroFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseSuperheroFields settings txts val = do
  superhero <- parseSuperhero settings val
  helper superhero txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedSuperheroField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> AesonKey
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
     (MonadIO m, MonadThrow m) => FakerSettings -> AesonKey -> m Text
resolveSuperheroText settings txt = genericResolver settings txt resolveSuperheroField

resolveSuperheroField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> AesonKey -> m Text
resolveSuperheroField settings field@"Superhero.prefix" =
  cachedRandomVec "superhero" field superheroPrefixProvider settings
resolveSuperheroField settings field@"Superhero.suffix" =
  cachedRandomVec "superhero" field superheroSuffixProvider settings
resolveSuperheroField settings field@"Superhero.descriptor" =
  cachedRandomVec "superhero" field superheroDescriptorProvider settings
resolveSuperheroField settings str = throwM $ InvalidField "superhero" str
