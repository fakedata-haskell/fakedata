{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Camera where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH


parseCamera :: FromJSON a => FakerSettings -> Value -> Parser a
parseCamera settings (Object obj) = do
  en <- obj .: (getLocaleKey settings)
  faker <- en .: "faker"
  camera <- faker .: "camera"
  pure camera
parseCamera settings val = fail $ "expected Object, but got " <> (show val)

parseCameraField ::
     (FromJSON a, Monoid a) => FakerSettings -> AesonKey -> Value -> Parser a
parseCameraField settings txt val = do
  camera <- parseCamera settings val
  field <- camera .:? txt .!= mempty
  pure field

parseCameraFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [AesonKey] -> Value -> Parser a
parseCameraFields settings txts val = do
  camera <- parseCamera settings val
  helper camera txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedCameraFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [AesonKey]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedCameraFields settings txts val = do
  camera <- parseCamera settings val
  helper camera txts
  where
    helper :: (FromJSON a) => Value -> [AesonKey] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)




$(genParser "camera" "brand")

$(genProvider "camera" "brand")

$(genParser "camera" "model")

$(genProvider "camera" "model")

$(genParser "camera" "brand_with_model")

$(genProvider "camera" "brand_with_model")
