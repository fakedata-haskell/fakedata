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
import Faker.Provider.Name (nameProvider, resolveNameField, resolveNameText)
import Faker.Provider.TH
import Language.Haskell.TH

parseBook :: FromJSON a => FakerSettings -> Value -> Parser a
parseBook settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  book <- faker .: "book"
  pure book
parseBook settings val = fail $ "expected Object, but got " <> (show val)

parseBookField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBookField settings txt val = do
  book <- parseBook settings val
  field <- book .:? txt .!= mempty
  pure field

parseUnresolvedBookField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
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

$(genParser "book" "publisher")

$(genProvider "book" "publisher")

$(genParser "book" "genre")

$(genProvider "book" "genre")

resolveBookText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveBookText settings txt = do
  let fields = resolveFields txt
  bookFields <- mapM (resolveBookField settings) fields
  pure $ operateFields txt bookFields

resolveBookField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveBookField settings "Name.name" =
  randomUnresolvedVec settings nameProvider resolveNameText
resolveBookField settings str = throwM $ InvalidField "book" str
