{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Book where

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
import Faker.Provider.Name (nameNameProvider, resolveNameField, resolveNameText)
import Faker.Provider.TH
import Language.Haskell.TH


parseBook :: FromJSON a => FakerSettings -> Value -> Parser a
parseBook settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  book <- faker .: "book"
  pure book
parseBook settings val = fail $ "expected Object, but got " <> (show val)

parseBookField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseBookField settings txt val = do
  book <- parseBook settings val
  field <- book .:? txt .!= mempty
  pure field

parseUnresolvedBookField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> AesonKey
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBookField settings txt val = do
  book <- parseBook settings val
  field <- book .:? txt .!= mempty
  pure $ pure field

$(genParser "book" "title")

$(genProvider "book" "title")

$(genParserUnresolved "book" "author")

bookAuthorProvider ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved Text)
bookAuthorProvider settings = fetchData settings Book parseBookAuthorUnresolved

parseAuthorFieldHyLocale ::
     (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
parseAuthorFieldHyLocale settings = parseBookField settings "author"

authorProviderHyLocale ::
     (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
authorProviderHyLocale settings =
  fetchData settings Address parseAuthorFieldHyLocale

$(genParser "book" "publisher")

$(genProvider "book" "publisher")

$(genParser "book" "genre")

$(genProvider "book" "genre")

authorResolver :: (MonadIO m, MonadThrow m) => FakerSettings -> m Text
authorResolver settings =
  case (getLocale settings) `elem` ["hy"] of
    True -> (cachedRandomVec "book" "author" authorProviderHyLocale settings)
    False ->
      (cachedRandomUnresolvedVecWithoutVector
         "book"
         "author"
         bookAuthorProvider
         resolveBookText
         settings)

resolveBookText :: (MonadIO m, MonadThrow m) => FakerSettings -> AesonKey -> m Text
resolveBookText = genericResolver' resolveBookField

resolveBookField :: (MonadThrow m, MonadIO m) => FakerSettings -> AesonKey -> m Text
resolveBookField settings "Name.name" =
  cachedRandomUnresolvedVec
    "name"
    "name"
    nameNameProvider
    resolveNameText
    settings
resolveBookField settings str = throwM $ InvalidField "book" str
