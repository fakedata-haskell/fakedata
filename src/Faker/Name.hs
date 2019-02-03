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
import Faker.Internal
import Faker.Provider.Name


maleFirstName :: Fake Text
maleFirstName = Fake (\settings -> randomVec settings maleFirstNameProvider)

femaleFirstName :: Fake Text
femaleFirstName = Fake (\settings -> randomVec settings femaleFirstNameProvider)

prefix :: Fake Text
prefix = Fake (\settings -> randomVec settings prefixProvider)

suffix :: Fake Text
suffix = Fake (\settings -> randomVec settings suffixProvider)

name :: Fake Text
name = Fake (\settings -> randomUnresolvedVec settings nameProvider resolveNameText)

nameWithMiddle :: Fake Text
nameWithMiddle = Fake (\settings -> randomUnresolvedVec settings nameWithMiddleProvider resolveNameText)
