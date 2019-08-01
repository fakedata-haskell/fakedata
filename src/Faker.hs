{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Faker
  (
    -- * Types
    Fake(..)
  , FakerSettings
  , FakerException(..)
  , defaultFakerSettings
    -- * Setters
  , setLocale
  , setRandomGen
  , setDeterministic
  , setNonDeterministic
    -- * Getters
  , getRandomGen
  , getLocale
  , getDeterministic
  , getCache
  , setCache
    -- * Generators
  , generate
  , generateWithSettings
  ) where

import Control.Exception (Exception)
import Control.Monad (ap)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text)
import Data.Typeable
import Data.Vector (Vector)
import System.Random (StdGen, mkStdGen, newStdGen, split)

data FakerSettings =
  FakerSettings
    { fslocale :: Text -- ^ Locale settings for your fake data source.
    , fsrandomGen :: StdGen -- ^ Standard random generator
    , fsDeterministic :: Bool -- ^ Controls whether you want
                            -- deterministic out. This overrides
                            -- fsrandomGen.
    , fsCache :: IORef (HM.HashMap (String, String, Text) (Vector Text)) -- String corresponds to Show instance of sourceData
    }

instance Show FakerSettings where
  show _ = "fs"

data FakerException
  = InvalidLocale String -- ^ This is thrown when it is not able to
                         -- find the fake data source for your
                         -- localization.
  | InvalidField String Text -- ^ The 'String' represents the field is
                             -- trying to resolve and the 'Text' field
                             -- is something you passed on.
  | NoDataFound FakerSettings -- ^ This is thrown when you have no
                              -- data. This may likely happen for
                              -- locales other than `en`.
  deriving (Typeable, Show)

instance Exception FakerException

-- | Default faker settings with locale of \"en\" and Deterministic output.
defaultFakerSettings :: FakerSettings
defaultFakerSettings =
  FakerSettings
    { fslocale = "en"
    , fsrandomGen = (mkStdGen 10000)
    , fsDeterministic = True
    , fsCache = error "defaultFakerSettings: Cache not initialized"
    }

-- | Sets the locale. Note that for any other locale apart from
-- \"en\", you need to make sure that the data is acutally present. In
-- case no data is found, 'NoDataFound' exception will be thrown. You
-- can check the presence of the data in a particular locale by
-- inspecting the `yml` file of the corresponding locale. The file
-- would be bundled along with the particular Hackage release.
setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs {fslocale = localeTxt}

-- | Sets the random generator
setRandomGen :: StdGen -> FakerSettings -> FakerSettings
setRandomGen gen fs = fs {fsrandomGen = gen}

-- | Get the random generator
getRandomGen :: FakerSettings -> StdGen
getRandomGen settings = fsrandomGen settings

-- | Get the Locale settings for your fake data source
getLocale :: FakerSettings -> Text
getLocale FakerSettings {..} = fslocale

-- | Set the output of fakedata to be deterministic. With this you
-- will get the same ouput for the functions every time.
--
-- @
-- λ> import qualified Faker.Name as FN
-- λ> :t FN.name
-- FN.name :: Fake Text
-- λ> generateWithSettings (setDeterministic defaultFakerSettings) FN.name
-- "Antony Langosh"
-- λ> generateWithSettings (setDeterministic defaultFakerSettings) FN.name
-- "Antony Langosh"
-- @
setDeterministic :: FakerSettings -> FakerSettings
setDeterministic fs = fs {fsDeterministic = True}

-- | Set the output of fakedata to be non deterministic. With this you
-- will get different ouput for the fake functions.
--
-- @
-- λ> generateWithSettings (setNonDeterministic defaultFakerSettings) FN.name
-- "Macy Shanahan"
-- λ> generateWithSettings (setNonDeterministic defaultFakerSettings) FN.name
-- "Rudy Dickinson II"
-- @
setNonDeterministic :: FakerSettings -> FakerSettings
setNonDeterministic fs = fs {fsDeterministic = False}

-- | Check if the fake data output is deterministic or not. A True
-- value indicates that it is deterministic.
getDeterministic :: FakerSettings -> Bool
getDeterministic FakerSettings {..} = fsDeterministic

getCache ::
     FakerSettings -> IO (HM.HashMap (String, String, Text) (Vector Text))
getCache FakerSettings {..} = readIORef fsCache

setCache ::
     HM.HashMap (String, String, Text) (Vector Text) -> FakerSettings -> IO ()
setCache cache fs = writeIORef (fsCache fs) cache

-- | Fake data type. This is the type you will be using to produce
-- fake values.
newtype Fake a =
  Fake
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
  liftIO xs = Fake (\_ -> xs >>= pure)

-- | Generate fake value with 'defaultFakerSettings'
--
-- @
-- λ> import qualified Faker.Name as FN
-- λ> generate FN.name
-- "Antony Langosh"
-- @
generate :: Fake a -> IO a
generate (Fake f) = do
  cache <- newIORef HM.empty
  f $ defaultFakerSettings {fsCache = cache}

-- | Generate fake value with supplied 'FakerSettings'
--
-- @
-- λ> generateWithSettings defaultFakerSettings FN.name
-- "Antony Langosh"
-- @
generateWithSettings :: FakerSettings -> Fake a -> IO a
generateWithSettings settings (Fake f) = do
  let deterministic = getDeterministic settings
  stdGen <-
    if deterministic
      then pure $ getRandomGen settings
      else newStdGen
  let newSettings = setRandomGen stdGen settings
  f newSettings
