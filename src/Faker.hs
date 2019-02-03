{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Faker
  ( Fake(..)
  , FakerSettings
  , defaultFakerSettings
  , setLocale
  , setRandomGen
  , getRandomGen
  , setDeterministic
  , setNonDeterministic
  , FakerException(..)
  , getLocale
  , Unresolved(..)
  , generate
  , generateWithSettings
  ) where

import Control.Exception (Exception)
import Control.Monad (ap)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Typeable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen, newStdGen, randomR, split)

data FakerSettings = FakerSettings
  { fslocale :: Text
  , fsrandomGen :: StdGen
  , fsDeterministic :: Bool
  } deriving (Show)

data FakerException
  = InvalidLocale String
  | InvalidField String
                 Text
  | NoDataFound FakerSettings
  deriving (Typeable, Show)

instance Exception FakerException

defaultFakerSettings :: FakerSettings
defaultFakerSettings =
  FakerSettings
    {fslocale = "en", fsrandomGen = (mkStdGen 10000), fsDeterministic = True}

setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs {fslocale = localeTxt}

setRandomGen :: StdGen -> FakerSettings -> FakerSettings
setRandomGen gen fs = fs {fsrandomGen = gen}

getRandomGen :: FakerSettings -> StdGen
getRandomGen settings = fsrandomGen settings

getLocale :: FakerSettings -> Text
getLocale FakerSettings {..} = fslocale

setDeterministic :: FakerSettings -> FakerSettings
setDeterministic fs = fs {fsDeterministic = True}

setNonDeterministic :: FakerSettings -> FakerSettings
setNonDeterministic fs = fs {fsDeterministic = False}

getDeterministic :: FakerSettings -> Bool
getDeterministic FakerSettings {..} = fsDeterministic

newtype Unresolved a = Unresolved
  { unresolvedField :: a
  } deriving (Functor)

instance Applicative Unresolved where
  pure = Unresolved
  Unresolved f1 <*> Unresolved f2 = pure $ f1 f2

instance Monad Unresolved where
  return = pure
  (Unresolved f) >>= f1 = f1 f

newtype Fake a = Fake
  { unFake :: FakerSettings -> IO a
  }

instance Functor Fake where
  fmap :: (a -> b) -> Fake a -> Fake b
  fmap f (Fake h) =
    Fake
      (\r -> do
         a <- h r
         let b = f a
         pure b)

instance Applicative Fake where
  pure x = Fake (\_ -> pure x)
  (<*>) = ap

instance Monad Fake where
  return :: a -> Fake a
  return x = Fake (\_ -> return x)
  (>>=) :: Fake a -> (a -> Fake b) -> Fake b
  (Fake h) >>= k =
    Fake
      (\settings ->
         let stdGen = getRandomGen settings
             (r1, _) = split stdGen
             m = do
               (item :: a) <- h settings
               let (Fake k1) = k item
               k1 (setRandomGen r1 settings)
          in m)

instance MonadIO Fake where
  liftIO :: IO a -> Fake a
  liftIO xs = Fake (\settings -> xs >>= pure)

generate :: Fake a -> IO a
generate (Fake f) = f defaultFakerSettings

generateWithSettings :: FakerSettings -> Fake a -> IO a
generateWithSettings settings (Fake f) = do
  let deterministic = getDeterministic settings
  stdGen <-
    if deterministic
      then pure $ getRandomGen settings
      else newStdGen
  let newSettings = setRandomGen stdGen settings
  f newSettings
