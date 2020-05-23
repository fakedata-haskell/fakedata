{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Internal module
module Faker.Internal
  ( Unresolved(..)
  , Regex(..)
  , rvec
  , insertToCache
  , presentInCache
  , resolver
  , refinedString
  , refinedText
  , operateFields
  , resolveFields
  , cachedRandomVec
  , cachedRandomUnresolvedVec
  , cachedRandomUnresolvedVecWithoutVector
  , cachedRegex
  , interpolateNumbers
  , interpolateString
  , resolveUnresolved
  , uncons2
  , operateField
  , modifyRandomGen
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
import Faker.Internal.Types (CacheFieldKey(..))
import System.Random (StdGen, mkStdGen, randomR, split)
import Text.StringRandom (stringRandom)

newtype Unresolved a = Unresolved
  { unresolvedField :: a
  } deriving (Functor)

newtype Regex = Regex { unRegex :: Text } deriving (Eq, Ord, Show)

instance Applicative Unresolved where
  pure = Unresolved
  Unresolved f1 <*> Unresolved f2 = pure $ f1 f2

instance Monad Unresolved where
  return = pure
  (Unresolved f) >>= f1 = f1 f

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
  -> Text
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
  -> Text
  -> (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> Text -> m Text)
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
  -> (FakerSettings -> Text -> m Text)
  -> m Text
randomUnresolvedVec settings provider resolverFn = do
  items <- provider settings
  resolveUnresolved settings items resolverFn

cachedRandomUnresolvedVecWithoutVector ::
     (MonadThrow m, MonadIO m)
  => Text
  -> Text
  -> (FakerSettings -> m (Unresolved Text))
  -> (FakerSettings -> Text -> m Text)
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
  -> (FakerSettings -> Text -> m Text)
  -> m Text
randomUnresolvedVecWithoutVector settings provider resolverFn = do
  items <- provider settings
  resolveUnresolved settings (sequenceA $ pure items) resolverFn

resolveUnresolved ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> Unresolved (Vector Text)
  -> (FakerSettings -> Text -> m Text)
  -> m Text
resolveUnresolved settings (Unresolved unres) resolverFn =
  let unresLen = V.length unres
      stdGen = getRandomGen settings
      (index, _) = randomR (0, unresLen - 1) stdGen
      randomItem = unres ! index
      resolve =
        if operateField randomItem "hello" == randomItem -- todo: remove hack
          then pure $
               interpolateString
                 settings
                 (interpolateNumbers settings randomItem)
          else resolverFn settings randomItem
   in if unresLen == 0
        then throwM $ NoDataFound settings
        else resolve

uncons2 :: Text -> Maybe (String, Text)
uncons2 txt = do
  (c1, rem1) <- T.uncons txt
  (c2, rem2) <- T.uncons rem1
  pure $ ((c1 : [c2]), rem2)

-- operateField "#{hello} #{world}" "jam"
-- "jam #{world}"
operateField :: Text -> Text -> Text
operateField origWord word = helper origWord word []
  where
    helper :: Text -> Text -> String -> Text
    helper txt word' acc =
      case uncons2 txt of
        Nothing -> origWord
        Just (str, rem') ->
          if str == "#{"
            then let actualRem = dropTillBrace rem'
                  in (T.pack acc) <> word' <> actualRem
            else case T.uncons txt of
                   Nothing -> origWord
                   Just (c, rem2) -> helper rem2 word' (acc <> [c])

operateFields :: Text -> [Text] -> Text
operateFields origWord [] = origWord
operateFields origWord (x:xs) = operateFields (operateField origWord x) xs

dropTillBrace :: Text -> Text
dropTillBrace txt = T.dropWhile (== '}') $ T.dropWhile (/= '}') txt

-- λ> extractSingleField "#{hello} #{world} jam"
-- ("hello"," #{world} jam")
extractSingleField :: Text -> (Text, Text)
extractSingleField txt =
  let (field, remaining) = T.span (\x -> x /= '}') txt''
   in (T.drop 2 field, T.drop 1 remaining)
  where
    txt' = strip txt
    txt'' = snd $ T.span (\x -> x /= '#') txt'

-- λ> resolveFields "#{hello} #{world}"
-- ["hello","world"]
-- λ> resolveFields "#{hello} #{world} jam"
-- ["hello","world"]
resolveFields :: Text -> [Text]
resolveFields txt =
  let (field, remaining) = extractSingleField txt
      newField val =
        if T.null val
          then []
          else [val]
   in case T.null remaining of
        True -> newField field
        False -> newField field <> resolveFields remaining

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

isHash :: Char -> Bool
isHash c = c == '#'

-- >> interpolateNumbers "#####"
-- >> 23456
-- >> interpolateNumbers "ab-##"
-- >> ab-32
interpolateNumbers :: FakerSettings -> Text -> Text
interpolateNumbers fsettings txt = helper fsettings [] txt
  where
    helper settings acc text =
      case T.null text of
        True -> T.pack acc
        False ->
          case T.uncons text of
            Nothing -> T.pack acc
            Just (c, rem') ->
              if isHash c
                then let stdGen = getRandomGen settings
                         (int, gen) = randomR (0, 9) stdGen
                      in helper
                           (setRandomGen gen settings)
                           (acc ++ [digitToChar int])
                           rem'
                else helper settings (acc ++ [c]) rem'

isQues :: Char -> Bool
isQues c = c == '?'

-- >> interpolateString "?????"
-- >> ABCDE
-- >> interpolateString "32-##"
-- >> 32-ZF
interpolateString :: FakerSettings -> Text -> Text
interpolateString fsettings text = helper fsettings [] text
  where
    helper :: FakerSettings -> [Char] -> Text -> Text
    helper settings acc txt =
      case T.null txt of
        True -> T.pack acc
        False ->
          case T.uncons txt of
            Nothing -> T.pack acc
            Just (c, remTxt) ->
              if isQues c
                then let stdGen = getRandomGen settings
                         (int, gen) = randomR ('A', 'Z') stdGen
                      in helper
                           (setRandomGen gen settings)
                           (acc ++ [int])
                           remTxt
                else helper settings (acc ++ [c]) remTxt

resolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Vector a))
  -> FakerSettings
  -> m a
resolver provider = \settings -> randomVec settings provider

unresolvedResolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> Text -> m Text)
  -> (FakerSettings -> m Text)
unresolvedResolver provider resolverFn =
  \settings -> randomUnresolvedVec settings provider resolverFn

unresolvedResolverWithoutVector ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Unresolved Text))
  -> (FakerSettings -> Text -> m Text)
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

presentInCache :: Text -> Text -> FakerSettings -> IO (Maybe (Vector Text))
presentInCache sdata field settings = do
  let key =
        CacheFieldKey
          {ckSource = sdata, ckLocale = getLocale settings, ckField = field}
  hmap <- getCacheField settings
  pure $ HM.lookup key hmap

insertToCache :: Text -> Text -> FakerSettings -> (Vector Text) -> IO ()
insertToCache sdata field settings vec = do
  let key =
        CacheFieldKey
          {ckSource = sdata, ckLocale = getLocale settings, ckField = field}
  hmap <- getCacheField settings
  let hmap2 = HM.insert key vec hmap
  setCacheField hmap2 settings

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
  -> Text
  -> (FakerSettings -> m Text)
  -> FakerSettings
  -> m Regex
cachedRegex sdata field provider settings = do
  val <- liftIO $ presentInCache sdata field settings
  case val of
    Nothing -> do
      dat <- provider settings
      liftIO $ insertToCache sdata field settings (V.singleton dat)
      generateRegexData settings provider
    Just vec -> pure $ generateRegex settings (V.head vec)

cleanFakerRegex :: Text -> Text
cleanFakerRegex xs = T.dropEnd 1 $ T.drop 1 xs

generateRegex :: FakerSettings -> Text -> Regex
generateRegex settings regex =
  let stdGen = getRandomGen settings
  in Regex $ stringRandom stdGen (cleanFakerRegex regex)

generateRegexData ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m Text)
  -> m Regex
generateRegexData settings provider = do
  items <- provider settings
  pure $ generateRegex settings items

