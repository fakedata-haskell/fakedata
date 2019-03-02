{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Markdown where

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

parseMarkdown :: FromJSON a => FakerSettings -> Value -> Parser a
parseMarkdown settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  markdown <- faker .: "markdown"
  pure markdown
parseMarkdown settings val = fail $ "expected Object, but got " <> (show val)

parseMarkdownField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseMarkdownField settings txt val = do
  markdown <- parseMarkdown settings val
  field <- markdown .:? txt .!= mempty
  pure field

parseMarkdownFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseMarkdownFields settings txts val = do
  markdown <- parseMarkdown settings val
  helper markdown txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

$(genParser "markdown" "headers")

$(genProvider "markdown" "headers")

$(genParser "markdown" "emphasis")

$(genProvider "markdown" "emphasis")
