{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

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
  , setCacheField
  , setCacheFile
  , replaceCacheField
  , replaceCacheFile
    -- * Getters
  , getRandomGen
  , getLocale
  , getDeterministic
  , getCacheField
  , getCacheFile
    -- * Generators
  , generate
  , generateNonDeterministic
  , generateWithSettings
  ) where

import Control.Exception (Exception)
import Control.Monad (ap)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Typeable
import Data.Vector (Vector)
import Data.Word (Word64)
import Data.Yaml (Value)
import Faker.Internal.Types (CacheFieldKey, CacheFileKey)
import System.Random (StdGen, mkStdGen, newStdGen, split)

data FakerSettings = FakerSettings
  { fslocale :: !Text -- ^ Locale settings for your fake data source.
  , fsrandomGen :: !StdGen -- ^ Seed to initialize random generator state
  , fsDeterministic :: !Bool -- ^ Controls whether you want
                            -- deterministic out. This overrides
                            -- 'fsrandomGen'.
  , fsCacheField :: (IORef (HM.HashMap CacheFieldKey (Vector Text)))
  , fsCacheFile :: (IORef (HM.HashMap CacheFileKey Value))
  }

newtype FakerGen = FakerGen
  { unFakerGen :: (Int, StdGen)
  } deriving (Show)

instance Show FakerSettings where
  show (FakerSettings {..}) =
    show fslocale ++ show fsrandomGen ++ show fsDeterministic

data FakerException
  = InvalidLocale String -- ^ This is thrown when it is not able to
                         -- find the fake data source for your
                         -- localization.
  | InvalidField String
                 Text -- ^ The 'String' represents the field it is
                             -- trying to resolve and the 'Text' field
                             -- is something you passed on.
  | NoDataFound FakerSettings -- ^ This is thrown when you have no
                              -- data. This may likely happen for
                              -- locales other than `en`.
  | ParseError String -- ^ This is thrown when the parsing step
                      -- fails. The 'String' represents the error
                      -- message.
  deriving (Typeable, Show)

instance Exception FakerException

-- | Default faker settings with locale of \"en\" and Deterministic output.
defaultFakerSettings :: FakerSettings
defaultFakerSettings =
  FakerSettings
    { fslocale = "en"
    , fsrandomGen = mkStdGen 10000
    , fsDeterministic = True
    , fsCacheField = error "defaultFakerSettings: fsCacheField not initialized"
    , fsCacheFile = error "defaultFakerSettings: fsCacheFile not initialized"
    }

-- | Sets the locale. Note that for any other locale apart from
-- \"en\", you need to make sure that the data is acutally present. In
-- case no data is found, 'NoDataFound' exception will be thrown. You
-- can check the presence of the data in a particular locale by
-- inspecting the `yml` file of the corresponding locale. The file
-- would be bundled along with the particular Hackage release.
setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs {fslocale = localeTxt}

-- | Sets the initial gen for random generator
setRandomGen :: StdGen -> FakerSettings -> FakerSettings
setRandomGen gen fs = fs {fsrandomGen = gen}

-- | Get the initial gen for random generator
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

getCacheField :: FakerSettings -> IO (HM.HashMap CacheFieldKey (Vector Text))
getCacheField FakerSettings {..} = readIORef fsCacheField

setCacheField ::
     HM.HashMap CacheFieldKey (Vector Text) -> FakerSettings -> IO ()
setCacheField cache fs = do
  writeIORef (fsCacheField fs) cache

replaceCacheField ::
     HM.HashMap CacheFieldKey (Vector Text) -> FakerSettings -> IO FakerSettings
replaceCacheField cache fs = do
  ref <- newIORef cache
  pure $ fs {fsCacheField = ref}

getCacheFile :: FakerSettings -> IO (HM.HashMap CacheFileKey Value)
getCacheFile FakerSettings {..} = readIORef fsCacheFile

setCacheFile :: HM.HashMap CacheFileKey Value -> FakerSettings -> IO ()
setCacheFile cache fs = writeIORef (fsCacheFile fs) cache

replaceCacheFile ::
     HM.HashMap CacheFileKey Value -> FakerSettings -> IO FakerSettings
replaceCacheFile cache fs = do
  ref <- newIORef cache
  pure $ fs {fsCacheFile = ref}

-- | Fake data type. This is the type you will be using to produce
-- fake values.
newtype Fake a = Fake
  { unFake :: FakerSettings -> IO a
  }

instance Functor Fake where
  {-# INLINE fmap #-}
  fmap :: (a -> b) -> Fake a -> Fake b
  fmap f (Fake h) =
    Fake
      (\r -> do
         a <- h r
         let b = f a
         pure b)

instance Applicative Fake where
  {-# INLINE pure #-}    
  pure x = Fake (\_ -> pure x)
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Fake where
  {-# INLINE return #-}
  return :: a -> Fake a
  return x = Fake (\_ -> return x)
  {-# INLINE (>>=) #-}
  (>>=) :: Fake a -> (a -> Fake b) -> Fake b
  f >>= k = generateNewFake f k

generateNewFake :: Fake a -> (a -> Fake b) -> Fake b
generateNewFake (Fake h) k = Fake (\settings -> do
  let deterministic = getDeterministic settings
      currentStdGen = getRandomGen settings
      newStdGen = if deterministic
                  then currentStdGen
                  else fst $ split currentStdGen
  item <- h settings
  let (Fake k1) = k item
  k1 (setRandomGen newStdGen settings))
{-# SPECIALIZE INLINE generateNewFake :: Fake Text -> (Text -> Fake Text) -> Fake Text #-}

instance MonadIO Fake where
  liftIO :: IO a -> Fake a
  liftIO xs = Fake (\_ -> xs >>= pure)

-- | @since 0.6.1
instance Semigroup a => Semigroup (Fake a) where
  mx <> my = (<>) <$> mx <*> my

-- | @since 0.6.1
instance Monoid a => Monoid (Fake a) where
  mempty = pure mempty
  mappend mx my = mappend <$> mx <*> my

-- | Generate fake value with 'defaultFakerSettings'
--
-- @
-- λ> import qualified Faker.Name as FN
-- λ> generate FN.name
-- "Antony Langosh"
-- @
generate :: Fake a -> IO a
generate (Fake f) = do
  cacheField <- newIORef HM.empty
  cacheFile <- newIORef HM.empty
  f $ defaultFakerSettings {fsCacheField = cacheField, fsCacheFile = cacheFile}

-- | Generate fake value with 'defaultFakerSettings' but with non
-- deterministic setting.
--
-- @since 0.8.0
--
-- @
-- λ> import qualified Faker.Name as FN
-- λ> generateNonDeterministic FN.name
-- "Prof. Antoine O'Conner"
-- λ> generateNonDeterministic FN.name
-- "Savannah Buckridge"
-- @
generateNonDeterministic :: Fake a -> IO a
generateNonDeterministic = generateWithSettings $ setNonDeterministic defaultFakerSettings

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
  cacheField <- newIORef HM.empty
  cacheFile <- newIORef HM.empty
  f $ newSettings {fsCacheField = cacheField, fsCacheFile = cacheFile}
