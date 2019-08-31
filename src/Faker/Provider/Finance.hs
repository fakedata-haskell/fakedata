{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Finance where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseFinance :: FromJSON a => FakerSettings -> Value -> Parser a
parseFinance settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  finance <- faker .: "finance"
  creditCard <- finance .: "credit_card"
  pure creditCard
parseFinance settings val = fail $ "expected Object, but got " <> (show val)

parseFinanceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseFinanceField settings txt val = do
  finance <- parseFinance settings val
  field <- finance .:? txt .!= mempty
  pure field

parseFinanceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseFinanceFields settings txts val = do
  finance <- parseFinance settings val
  helper finance txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedFinanceField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedFinanceField settings txt val = do
  finance <- parseFinance settings val
  field <- finance .:? txt .!= mempty
  pure $ pure field

$(genParserUnresolved "finance" "visa")

$(genProviderUnresolved "finance" "visa")

$(genParserUnresolved "finance" "mastercard")

$(genProviderUnresolved "finance" "mastercard")

$(genParserUnresolved "finance" "discover")

$(genProviderUnresolved "finance" "discover")

$(genParserUnresolved "finance" "american_express")

$(genProviderUnresolved "finance" "american_express")

$(genParserUnresolved "finance" "diners_club")

$(genProviderUnresolved "finance" "diners_club")

$(genParserUnresolved "finance" "jcb")

$(genProviderUnresolved "finance" "jcb")

$(genParserUnresolved "finance" "switch")

$(genProviderUnresolved "finance" "switch")

$(genParserUnresolved "finance" "solo")

$(genProviderUnresolved "finance" "solo")

$(genParserUnresolved "finance" "dankort")

$(genProviderUnresolved "finance" "dankort")

$(genParserUnresolved "finance" "maestro")

$(genProviderUnresolved "finance" "maestro")

$(genParserUnresolved "finance" "forbrugsforeningen")

$(genProviderUnresolved "finance" "forbrugsforeningen")

$(genParserUnresolved "finance" "laser")

$(genProviderUnresolved "finance" "laser")

resolveFinanceText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveFinanceText settings txt = do
  let fields = resolveFields txt
  financeFields <- mapM (resolveFinanceField settings) fields
  pure $ operateFields txt financeFields

resolveFinanceField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveFinanceField settings field@"visa" =
  cachedRandomUnresolvedVec
    "finance"
    field
    financeVisaProvider
    resolveFinanceField
    settings
resolveFinanceField settings field@"mastercard" =
  cachedRandomUnresolvedVec
    "finance"
    field
    financeMastercardProvider
    resolveFinanceField
    settings
resolveFinanceField settings field@"discover" =
  cachedRandomUnresolvedVec
    "finance"
    field
    financeDiscoverProvider
    resolveFinanceField
    settings
resolveFinanceField settings str = throwM $ InvalidField "finance" str
