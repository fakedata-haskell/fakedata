{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
{-#LANGUAGE BangPatterns#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Faker.Name where

import Data.Yaml
import Faker
import Config
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import Control.Monad.Catch
import Data.Text
import System.Directory (doesFileExist)
import System.FilePath
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Random
import Debug.Trace

parseName :: FromJSON a => Value -> Parser a
parseName (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  name <- faker .: "name"
  pure name
parseName val = fail $ "expected Object, but got " <> (show val)

parseNameField :: FromJSON a => Text -> Value -> Parser a
parseNameField txt val = do
  name <- parseName val
  field <- name .: txt
  pure field

parseUnresolvedNameField :: FromJSON a => Text -> Value -> Parser (Unresolved a)
parseUnresolvedNameField txt val = do
  name <- parseName val
  field <- name .: txt
  pure $ pure field

parseMaleFirstName :: FromJSON a => Value -> Parser a
parseMaleFirstName = parseNameField "male_first_name"

parseFemaleFirstName :: FromJSON a => Value -> Parser a
parseFemaleFirstName = parseNameField "female_first_name"

parseFirstName :: FromJSON a => Value -> Parser (Unresolved a)
parseFirstName = parseUnresolvedNameField "first_name"

parseLastName :: FromJSON a => Value -> Parser a
parseLastName = parseNameField "last_name"

parsePrefix :: FromJSON a => Value -> Parser a
parsePrefix = parseNameField "prefix"

parseSuffix :: FromJSON a => Value -> Parser a
parseSuffix = parseNameField "suffix"

parseFieldName :: FromJSON a => Value -> Parser (Unresolved a)
parseFieldName = parseUnresolvedNameField "name"

parseNameWithMiddle :: FromJSON a => Value -> Parser (Unresolved a)
parseNameWithMiddle = parseUnresolvedNameField "name_with_middle"

maleFirstNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
maleFirstNameProvider settings = fetchData settings Name parseMaleFirstName

femaleFirstNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
femaleFirstNameProvider settings = fetchData settings Name parseFemaleFirstName

firstNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
firstNameProvider settings = fetchData settings Name parseFirstName

lastNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
lastNameProvider settings = fetchData settings Name parseLastName

prefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
prefixProvider settings = fetchData settings Name parsePrefix

suffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
suffixProvider settings = fetchData settings Name parseSuffix

nameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
nameProvider settings = fetchData settings Name parseFieldName

nameWithMiddleProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
nameWithMiddleProvider settings = fetchData settings Name parseNameWithMiddle


resolveNameField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveNameField settings "female_first_name" = randomVec settings femaleFirstNameProvider
resolveNameField settings "male_first_name" = randomVec settings maleFirstNameProvider
resolveNameField settings "prefix" = randomVec settings prefixProvider
resolveNameField settings "suffix" = randomVec settings suffixProvider
resolveNameField settings "first_name" = randomUnresolvedVec settings firstNameProvider resolveNameField
resolveNameField settings "last_name" = randomVec settings lastNameProvider
resolveNameField settings str = throwM $ InvalidField "name" str
