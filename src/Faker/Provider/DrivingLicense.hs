{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.DrivingLicense where

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

parseDrivingLicense :: FromJSON a => FakerSettings -> Value -> Parser a
parseDrivingLicense settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  drivingLicense <- faker .: "driving_licence"
  pure drivingLicense
parseDrivingLicense settings val = fail $ "expected Object, but got " <> (show val)

parseDrivingLicenseField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseDrivingLicenseField settings txt val = do
  drivingLicense <- parseDrivingLicense settings val
  field <- drivingLicense .:? txt .!= mempty
  pure field

parseDrivingLicenseFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseDrivingLicenseFields settings txts val = do
  drivingLicense <- parseDrivingLicense settings val
  helper drivingLicense txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)


parseUnresolvedDrivingLicenseField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedDrivingLicenseField settings txt val = do
  drivingLicense <- parseDrivingLicense settings val
  field <- drivingLicense .:? txt .!= mempty
  pure $ pure field



parseUnresolvedDrivingLicenseFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedDrivingLicenseFields settings txts val = do
  drivingLicense <- parseDrivingLicense settings val
  helper drivingLicense txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)


$(genParserUnresolveds "drivingLicense" ["usa","alabama"])
$(genProviderUnresolveds "drivingLicense" ["usa","alabama"])

$(genParserUnresolveds "drivingLicense" ["usa","alaska"])
$(genProviderUnresolveds "drivingLicense" ["usa","alaska"])

$(genParserUnresolveds "drivingLicense" ["usa","arizona"])
$(genProviderUnresolveds "drivingLicense" ["usa","arizona"])

$(genParserUnresolveds "drivingLicense" ["usa","arkansas"])
$(genProviderUnresolveds "drivingLicense" ["usa","arkansas"])

$(genParserUnresolveds "drivingLicense" ["usa","california"])
$(genProviderUnresolveds "drivingLicense" ["usa","california"])

$(genParserUnresolveds "drivingLicense" ["usa","colorado"])
$(genProviderUnresolveds "drivingLicense" ["usa","colorado"])

$(genParserUnresolveds "drivingLicense" ["usa","connecticut"])
$(genProviderUnresolveds "drivingLicense" ["usa","connecticut"])

$(genParserUnresolveds "drivingLicense" ["usa","delaware"])
$(genProviderUnresolveds "drivingLicense" ["usa","delaware"])

$(genParserUnresolveds "drivingLicense" ["usa","district_of_columbia"])
$(genProviderUnresolveds "drivingLicense" ["usa","district_of_columbia"])

$(genParserUnresolveds "drivingLicense" ["usa","florida"])
$(genProviderUnresolveds "drivingLicense" ["usa","florida"])

$(genParserUnresolveds "drivingLicense" ["usa","georgia"])
$(genProviderUnresolveds "drivingLicense" ["usa","georgia"])

$(genParserUnresolveds "drivingLicense" ["usa","hawaii"])
$(genProviderUnresolveds "drivingLicense" ["usa","hawaii"])

$(genParserUnresolveds "drivingLicense" ["usa","idaho"])
$(genProviderUnresolveds "drivingLicense" ["usa","idaho"])

$(genParserUnresolveds "drivingLicense" ["usa","illinois"])
$(genProviderUnresolveds "drivingLicense" ["usa","illinois"])

$(genParserUnresolveds "drivingLicense" ["usa","indiana"])
$(genProviderUnresolveds "drivingLicense" ["usa","indiana"])

$(genParserUnresolveds "drivingLicense" ["usa","iowa"])
$(genProviderUnresolveds "drivingLicense" ["usa","iowa"])

$(genParserUnresolveds "drivingLicense" ["usa","kansas"])
$(genProviderUnresolveds "drivingLicense" ["usa","kansas"])

$(genParserUnresolveds "drivingLicense" ["usa","kentucky"])
$(genProviderUnresolveds "drivingLicense" ["usa","kentucky"])

$(genParserUnresolveds "drivingLicense" ["usa","louisiana"])
$(genProviderUnresolveds "drivingLicense" ["usa","louisiana"])

$(genParserUnresolveds "drivingLicense" ["usa","maine"])
$(genProviderUnresolveds "drivingLicense" ["usa","maine"])

$(genParserUnresolveds "drivingLicense" ["usa","maryland"])
$(genProviderUnresolveds "drivingLicense" ["usa","maryland"])

$(genParserUnresolveds "drivingLicense" ["usa","massachusetts"])
$(genProviderUnresolveds "drivingLicense" ["usa","massachusetts"])

$(genParserUnresolveds "drivingLicense" ["usa","michigan"])
$(genProviderUnresolveds "drivingLicense" ["usa","michigan"])

$(genParserUnresolveds "drivingLicense" ["usa","minnesota"])
$(genProviderUnresolveds "drivingLicense" ["usa","minnesota"])

$(genParserUnresolveds "drivingLicense" ["usa","mississippi"])
$(genProviderUnresolveds "drivingLicense" ["usa","mississippi"])

$(genParserUnresolveds "drivingLicense" ["usa","missouri"])
$(genProviderUnresolveds "drivingLicense" ["usa","missouri"])

$(genParserUnresolveds "drivingLicense" ["usa","montana"])
$(genProviderUnresolveds "drivingLicense" ["usa","montana"])

$(genParserUnresolveds "drivingLicense" ["usa","nebraska"])
$(genProviderUnresolveds "drivingLicense" ["usa","nebraska"])

$(genParserUnresolveds "drivingLicense" ["usa","nevada"])
$(genProviderUnresolveds "drivingLicense" ["usa","nevada"])

$(genParserUnresolveds "drivingLicense" ["usa","new_hampshire"])
$(genProviderUnresolveds "drivingLicense" ["usa","new_hampshire"])

$(genParserUnresolveds "drivingLicense" ["usa","new_jersey"])
$(genProviderUnresolveds "drivingLicense" ["usa","new_jersey"])

$(genParserUnresolveds "drivingLicense" ["usa","new_mexico"])
$(genProviderUnresolveds "drivingLicense" ["usa","new_mexico"])

$(genParserUnresolveds "drivingLicense" ["usa","new_york"])
$(genProviderUnresolveds "drivingLicense" ["usa","new_york"])

$(genParserUnresolveds "drivingLicense" ["usa","north_carolina"])
$(genProviderUnresolveds "drivingLicense" ["usa","north_carolina"])

$(genParserUnresolveds "drivingLicense" ["usa","ohio"])
$(genProviderUnresolveds "drivingLicense" ["usa","ohio"])

$(genParserUnresolveds "drivingLicense" ["usa","oklahoma"])
$(genProviderUnresolveds "drivingLicense" ["usa","oklahoma"])

$(genParserUnresolveds "drivingLicense" ["usa","oregon"])
$(genProviderUnresolveds "drivingLicense" ["usa","oregon"])

$(genParserUnresolveds "drivingLicense" ["usa","pennsylvania"])
$(genProviderUnresolveds "drivingLicense" ["usa","pennsylvania"])

$(genParserUnresolveds "drivingLicense" ["usa","rhode_island"])
$(genProviderUnresolveds "drivingLicense" ["usa","rhode_island"])

$(genParserUnresolveds "drivingLicense" ["usa","south_carolina"])
$(genProviderUnresolveds "drivingLicense" ["usa","south_carolina"])

$(genParserUnresolveds "drivingLicense" ["usa","south_dakota"])
$(genProviderUnresolveds "drivingLicense" ["usa","south_dakota"])

$(genParserUnresolveds "drivingLicense" ["usa","tennessee"])
$(genProviderUnresolveds "drivingLicense" ["usa","tennessee"])

$(genParserUnresolveds "drivingLicense" ["usa","texas"])
$(genProviderUnresolveds "drivingLicense" ["usa","texas"])

$(genParserUnresolveds "drivingLicense" ["usa","utah"])
$(genProviderUnresolveds "drivingLicense" ["usa","utah"])

$(genParserUnresolveds "drivingLicense" ["usa","vermont"])
$(genProviderUnresolveds "drivingLicense" ["usa","vermont"])

$(genParserUnresolveds "drivingLicense" ["usa","virginia"])
$(genProviderUnresolveds "drivingLicense" ["usa","virginia"])

$(genParserUnresolveds "drivingLicense" ["usa","washington"])
$(genProviderUnresolveds "drivingLicense" ["usa","washington"])

$(genParserUnresolveds "drivingLicense" ["usa","west_virginia"])
$(genProviderUnresolveds "drivingLicense" ["usa","west_virginia"])

$(genParserUnresolveds "drivingLicense" ["usa","wisconsin"])
$(genProviderUnresolveds "drivingLicense" ["usa","wisconsin"])

$(genParserUnresolveds "drivingLicense" ["usa","wyoming"])
$(genProviderUnresolveds "drivingLicense" ["usa","wyoming"])

resolveDrivingLicenseText :: (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveDrivingLicenseText = genericResolver' resolveDrivingLicenseField

resolveDrivingLicenseField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text

resolveDrivingLicenseField settings str = throwM $ InvalidField "drivingLicense" str
