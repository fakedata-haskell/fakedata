{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.University where

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
import Faker.Provider.Address (stateProvider)
import Faker.Provider.Name (lastNameProvider)
import Faker.Provider.TH
import Language.Haskell.TH

parseUniversity :: FromJSON a => FakerSettings -> Value -> Parser a
parseUniversity settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  university <- faker .: "university"
  pure university
parseUniversity settings val = fail $ "expected Object, but got " <> (show val)

parseUniversityField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseUniversityField settings txt val = do
  university <- parseUniversity settings val
  field <- university .:? txt .!= mempty
  pure field

parseUniversityFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseUniversityFields settings txts val = do
  university <- parseUniversity settings val
  helper university txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedUniversityField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedUniversityField settings txt val = do
  university <- parseUniversity settings val
  field <- university .:? txt .!= mempty
  pure $ pure field

$(genParser "university" "prefix")

$(genProvider "university" "prefix")

$(genParser "university" "suffix")

$(genProvider "university" "suffix")

$(genParserUnresolved "university" "name")

$(genProviderUnresolved "university" "name")

resolveUniversityText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveUniversityText settings txt = do
  let fields = resolveFields txt
  universityFields <- mapM (resolveUniversityField settings) fields
  pure $ operateFields txt universityFields

resolveUniversityField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveUniversityField settings "University.prefix" =
  randomVec settings universityPrefixProvider
resolveUniversityField settings "University.suffix" =
  randomVec settings universitySuffixProvider
resolveUniversityField settings "Name.last_name" =
  randomVec settings lastNameProvider
resolveUniversityField settings "Address.state" =
  randomVec settings stateProvider
resolveUniversityField settings str = throwM $ InvalidField "university" str
