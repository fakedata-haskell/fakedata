{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE OverloadedStrings#-}

module Faker
    ( FakerSettings,
      defaultFakerSettings,
      setLocale,
      FakerException (..),
      fakerLocale
    ) where

import Control.Exception (Exception)
import Data.Text
import Data.Typeable

data FakerSettings = FakerSettings { locale :: Text } deriving (Show, Eq, Ord)

data FakerException = InvalidLocale String deriving (Typeable, Show)
instance Exception FakerException

defaultFakerSettings :: FakerSettings
defaultFakerSettings = FakerSettings { locale = "en" }

setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs { locale = localeTxt }

fakerLocale :: FakerSettings -> Text
fakerLocale FakerSettings {..} = locale
