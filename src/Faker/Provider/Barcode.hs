{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Barcode where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseBarcode :: FromJSON a => FakerSettings -> Value -> Parser a
parseBarcode settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  barcode <- faker .: "barcode"
  pure barcode
parseBarcode settings val = fail $ "expected Object, but got " <> (show val)

parseBarcodeField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseBarcodeField settings txt val = do
  barcode <- parseBarcode settings val
  field <- barcode .:? txt .!= mempty
  pure field

parseBarcodeFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseBarcodeFields settings txts val = do
  barcode <- parseBarcode settings val
  helper barcode txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedBarcodeField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedBarcodeField settings txt val = do
  barcode <- parseBarcode settings val
  field <- barcode .:? txt .!= mempty
  pure $ pure field

-- Generates parseBarcodeEan8Unresolved :: FakerSettings -> Value -> Parser (Unresolved Text)
$(genParserSingleUnresolved "barcode" "ean_8")

$(genProvidersSingleUnresolved "barcode" ["ean_8"])

$(genParserSingleUnresolved "barcode" "ean_13")

$(genProvidersSingleUnresolved "barcode" ["ean_13"])

$(genParserSingleUnresolved "barcode" "upc_a")

$(genProvidersSingleUnresolved "barcode" ["upc_a"])

$(genParserUnresolved "barcode" "upc_e")

$(genProviderUnresolved "barcode" "upc_e")

$(genParserUnresolved "barcode" "composite_symbol")

$(genProviderUnresolved "barcode" "composite_symbol")

$(genParserUnresolved "barcode" "isbn")

$(genProviderUnresolved "barcode" "isbn")

$(genParserSingleUnresolved "barcode" "ismn")

$(genProvidersSingleUnresolved "barcode" ["ismn"])

$(genParserSingleUnresolved "barcode" "issn")

$(genProvidersSingleUnresolved "barcode" ["issn"])

resolveBarcodeText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveBarcodeText = genericResolver' resolveBarcodeField

resolveBarcodeField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveBarcodeField settings str = throwM $ InvalidField "barcode" str
