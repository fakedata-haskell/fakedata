{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE OverloadedStrings#-}

module Faker
    ( FakerSettings,
      defaultFakerSettings,
      setLocale,
      setRandomGen,
      FakerException (..),
      fakerLocale
    ) where

import Control.Exception (Exception)
import Data.Text
import Data.Typeable
import System.Random

data FakerSettings = FakerSettings { locale :: Text, randomGen :: StdGen } deriving (Show)

data FakerException = InvalidLocale String
                    | InvalidField String Text
                      deriving (Typeable, Show)

instance Exception FakerException

defaultFakerSettings :: FakerSettings
defaultFakerSettings = FakerSettings { locale = "en", randomGen = (mkStdGen 10000) }

setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs { locale = localeTxt }

setRandomGen :: StdGen -> FakerSettings -> FakerSettings
setRandomGen gen fs = fs { randomGen = gen}

fakerLocale :: FakerSettings -> Text
fakerLocale FakerSettings {..} = locale
