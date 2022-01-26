{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Internal module
module Faker.Internal
  ( Unresolved(..)
  , Regex(..)
  , RegexFakeValue(..)
  , AesonKey(..)
  , rvec
  , insertToCache
  , presentInCache
  , resolver
  , refinedString
  , refinedText
  , cachedRandomVec
  , cachedRandomUnresolvedVec
  , cachedRandomUnresolvedVecWithoutVector
  , cachedRegex
  , resolveUnresolved
  , modifyRandomGen
  , resolveFields
  , genericResolver
  , genericResolver'
  , getLocaleKey
  ) where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char (toUpper)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text, strip)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.Word (Word64)
import Faker
import Faker.Internal.Types (CacheFieldKey(..), AesonKey(..)
                            , aesonKeyFromText
                            , aesonKeyToText)
import System.Random (StdGen, mkStdGen, randomR, split)
import Text.StringRandom (stringRandom)
import Fakedata.Parser
import Data.Attoparsec.Text as P
import Control.Monad (when)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
#endif

newtype Unresolved a = Unresolved
  { unresolvedField :: a
  } deriving (Functor)

newtype Regex = Regex { unRegex :: Text } deriving (Eq, Ord, Show)

newtype RegexFakeValue = RegexFakeValue { unRegexFakeValue :: Text } deriving (Eq, Ord, Show)

instance Applicative Unresolved where
  pure = Unresolved
  Unresolved f1 <*> Unresolved f2 = pure $ f1 f2

instance Monad Unresolved where
  return = pure
  (Unresolved f) >>= f1 = f1 f

-- | Get the Locale settings for your fake data source as a YAML  key.
getLocaleKey :: FakerSettings -> AesonKey
#if MIN_VERSION_aeson(2,0,0)
getLocaleKey settings = K.fromText (getLocale settings)
#else
getLocaleKey settings = getLocale settings
#endif

-- These are the functions which needs to be remodified
-- rvec, randomVec, randomUnresolvedvec, randomUnresolvedVecwithoutvector, unresolvedResolver, unresolfvedResolverWithoutVector
rvec :: (MonadThrow m, MonadIO m) => FakerSettings -> Vector a -> m a
rvec settings vec =
  let itemsLen = V.length vec
      (index, _) = randomR (0, itemsLen - 1) (getRandomGen settings)
   in if itemsLen == 0
        then throwM $ NoDataFound settings
        else pure $ vec ! index

cachedRandomVec ::
     (MonadThrow m, MonadIO m)
  => Text
  -> AesonKey
  -> (FakerSettings -> m (Vector Text))
  -> FakerSettings
  -> m Text
cachedRandomVec sdata field provider settings = do
  val <- liftIO $ presentInCache sdata field settings
  case val of
    Nothing -> do
      dat <- provider settings
      liftIO $ insertToCache sdata field settings dat
      randomVec settings (\_ -> pure dat)
    Just vec -> randomVec settings (\_ -> pure vec)

randomVec ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m (Vector a))
  -> m a
randomVec settings provider = do
  items <- provider settings
  let itemsLen = V.length items
      stdGen = getRandomGen settings
      (index, _) = randomR (0, itemsLen - 1) stdGen
  if itemsLen == 0
    then throwM $ NoDataFound settings
    else pure $ items ! index

cachedRandomUnresolvedVec ::
     (MonadThrow m, MonadIO m)
  => Text
  -> AesonKey
  -> (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> AesonKey -> m Text)
  -> FakerSettings
  -> m Text
cachedRandomUnresolvedVec sdata field provider resolverFn settings = do
  val <- liftIO $ presentInCache sdata field settings
  case val of
    Nothing -> do
      dat <- provider settings
      liftIO $ insertToCache sdata field settings (unresolvedField dat)
      resolveUnresolved settings dat resolverFn
    Just vec -> do
      let unres = Unresolved {unresolvedField = vec}
      randomUnresolvedVec settings (\_ -> pure unres) resolverFn

randomUnresolvedVec ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> AesonKey -> m Text)
  -> m Text
randomUnresolvedVec settings provider resolverFn = do
  items <- provider settings
  resolveUnresolved settings items resolverFn

cachedRandomUnresolvedVecWithoutVector ::
     (MonadThrow m, MonadIO m)
  => Text
  -> AesonKey
  -> (FakerSettings -> m (Unresolved Text))
  -> (FakerSettings -> AesonKey -> m Text)
  -> FakerSettings
  -> m Text
cachedRandomUnresolvedVecWithoutVector sdata field provider resolverFn settings = do
  val <- liftIO $ presentInCache sdata field settings
  case val of
    Nothing -> do
      dat <- provider settings
      liftIO $ insertToCache sdata field settings (pure $ unresolvedField dat)
      resolveUnresolved settings (sequenceA $ pure dat) resolverFn
    Just vec -> do
      let unres = Unresolved {unresolvedField = vec}
      randomUnresolvedVec settings (\_ -> pure unres) resolverFn

randomUnresolvedVecWithoutVector ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m (Unresolved Text))
  -> (FakerSettings -> AesonKey -> m Text)
  -> m Text
randomUnresolvedVecWithoutVector settings provider resolverFn = do
  items <- provider settings
  resolveUnresolved settings (sequenceA $ pure items) resolverFn

resolveUnresolved ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> Unresolved (Vector Text)
  -> (FakerSettings -> AesonKey -> m Text)
  -> m Text
resolveUnresolved settings (Unresolved unres) resolverFn = do
  let unresLen = V.length unres
      stdGen = getRandomGen settings
      (index, _) = randomR (0, unresLen - 1) stdGen
      randomItem = unres ! index
  when (unresLen == 0) $ throwM $ NoDataFound settings
  case P.parseOnly parseFakedata randomItem of
       Left err -> throwM $ ParseError err
       Right vals -> combineFakeIRValue settings resolverFn vals

resolveFakeIRValue :: (MonadIO m) => FakerSettings -> (FakerSettings -> AesonKey -> m Text) -> (FakeIRValue,StdGen) -> m Text
resolveFakeIRValue _ _ (Literal txt,_) = pure txt
resolveFakeIRValue settings _ (Hash num,_) = pure $ resolveHash settings num
resolveFakeIRValue settings _ (Ques num,_) = pure $ resolveQues settings num
resolveFakeIRValue settings resolverFn (Resolve text,gen) = resolverFn (setRandomGen gen settings) (aesonKeyFromText text)

combineFakeIRValue :: (MonadIO m) => FakerSettings -> (FakerSettings -> AesonKey -> m Text) -> [FakeIRValue] -> m Text
combineFakeIRValue settings resolverFn xs = do
  vals <- mapM (resolveFakeIRValue settings resolverFn) (zip xs (stdgens (getRandomGen settings)))
  pure $ T.concat vals

resolveFields :: (MonadIO m, MonadThrow m) => Text -> m [FakeIRValue]
resolveFields text = case P.parseOnly parseFakedata text of
                       Left err -> throwM $ ParseError err
                       Right vals -> pure vals

genericResolver :: (MonadIO m, MonadThrow m) => FakerSettings -> AesonKey -> (FakerSettings -> AesonKey -> m Text) -> m Text
genericResolver settings txt resolverFn = combineFakeIRValue settings resolverFn [Resolve (aesonKeyToText txt)]

genericResolver' :: (MonadIO m, MonadThrow m) => (FakerSettings -> AesonKey -> m Text) -> FakerSettings -> AesonKey -> m Text
genericResolver' resolverFn settings txt = genericResolver settings txt resolverFn

-- resolveHash settings 3
-- "234"
resolveHash :: FakerSettings -> Int -> Text
resolveHash settings num = T.pack $ helper settings num mempty
    where
      helper _ 0 acc = acc
      helper settings !n acc = do
        let (num, newGen) = randomR (0, 9) (getRandomGen settings)
        helper (setRandomGen newGen settings) (n - 1) ((digitToChar num):acc)

resolveQues :: FakerSettings -> Int -> Text
resolveQues settings num = T.pack $ helper settings num mempty
    where
      helper _ 0 acc = acc
      helper settings !n acc = do
        let (char, newGen) = randomR ('A', 'Z') (getRandomGen settings)
        helper (setRandomGen newGen settings) (n - 1) (char:acc)

digitToChar :: Int -> Char
digitToChar 0 = '0'
digitToChar 1 = '1'
digitToChar 2 = '2'
digitToChar 3 = '3'
digitToChar 4 = '4'
digitToChar 5 = '5'
digitToChar 6 = '6'
digitToChar 7 = '7'
digitToChar 8 = '8'
digitToChar 9 = '9'
digitToChar x = error $ "Expected single digit number, but received " <> show x

resolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Vector a))
  -> FakerSettings
  -> m a
resolver provider = \settings -> randomVec settings provider

unresolvedResolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> AesonKey -> m Text)
  -> (FakerSettings -> m Text)
unresolvedResolver provider resolverFn =
  \settings -> randomUnresolvedVec settings provider resolverFn

unresolvedResolverWithoutVector ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Unresolved Text))
  -> (FakerSettings -> AesonKey -> m Text)
  -> (FakerSettings -> m Text)
unresolvedResolverWithoutVector provider resolverFn =
  \settings -> randomUnresolvedVecWithoutVector settings provider resolverFn

uprStr :: String -> String
uprStr [] = []
uprStr (x:xs) = toUpper x : xs

refinedString :: String -> String
refinedString xs = aux xs []
  where
    whiteListChars :: [Char] = ['-', '_', ' ']
    aux :: String -> String -> String
    aux [] acc = acc
    aux (x:remTxt) acc =
      if x `elem` whiteListChars
        then if null remTxt
               then aux remTxt acc
               else aux (uprStr remTxt) acc
        else aux remTxt (acc ++ [x])

refinedText :: Text -> Text
refinedText = T.pack . refinedString . T.unpack

presentInCache :: Text -> AesonKey -> FakerSettings -> IO (Maybe (Vector Text))
presentInCache sdata field settings = do
  let key =
        CacheFieldKey
          {ckSource = sdata, ckLocale = getLocale settings, ckField = field}
  hmap <- getCacheField settings
  pure $ HM.lookup key hmap

insertToCache :: Text -> AesonKey -> FakerSettings -> (Vector Text) -> IO ()
insertToCache sdata field settings vec = do
  let key =
        CacheFieldKey
          {ckSource = sdata, ckLocale = getLocale settings, ckField = field}
  hmap <- getCacheField settings
  let hmap2 = HM.insert key vec hmap
  setCacheField hmap2 settings

stdgens :: StdGen -> [StdGen]
stdgens gen = let (g1, g2) = split gen
              in [gen, g1, g2] <> (stdgens g2)

-- TODO: Not efficient. Better to switch to splitmax once this gets
-- resolved: https://github.com/phadej/splitmix/issues/23
incrementStdGen :: Word64 -> StdGen -> StdGen
incrementStdGen 0 gen = gen
incrementStdGen n gen =
  let (_, newGen) = split gen
   in incrementStdGen (n - 1) newGen

modifyRandomGen :: FakerSettings -> Word64 -> FakerSettings
modifyRandomGen settings seed =
  let gen = getRandomGen settings
      newGen = incrementStdGen seed gen
   in setRandomGen newGen settings

cachedRegex ::
     (MonadThrow m, MonadIO m)
  => Text
  -> AesonKey
  -> (FakerSettings -> m Regex)
  -> FakerSettings
  -> m RegexFakeValue
cachedRegex sdata field provider settings = do
  val <- liftIO $ presentInCache sdata field settings
  case val of
    Nothing -> do
      dat <- provider settings
      liftIO $ insertToCache sdata field settings (V.singleton $ unRegex dat)
      generateRegexData settings provider
    Just vec -> pure $ generateRegex settings (Regex $ V.head vec)

cleanFakerRegex :: Text -> Text
cleanFakerRegex xs = T.dropEnd 1 $ T.drop 1 xs

generateRegex :: FakerSettings -> Regex -> RegexFakeValue
generateRegex settings regex =
  let stdGen = getRandomGen settings
  in RegexFakeValue $ stringRandom stdGen (cleanFakerRegex $ unRegex regex)

generateRegexData ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m Regex)
  -> m RegexFakeValue
generateRegexData settings provider = do
  items <- provider settings
  pure $ generateRegex settings items
