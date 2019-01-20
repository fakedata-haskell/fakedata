{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Faker.Address where

import Data.Yaml
import Faker
import Config
import Data.Vector
import Control.Monad.Catch
import Data.Text
import System.Directory (doesFileExist)
import System.FilePath
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Random

parseCountry :: Value -> Parser (Vector Text)
parseCountry (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  address <- faker .: "address"
  country <- address .: "country"
  pure country
parseCountry val = fail $ "expected Object, but got " <> (show val)

addressFileEn :: FilePath
addressFileEn = localesEnDirectory </> "address.yml"

guessAddressFile :: Text -> FilePath
guessAddressFile sysloc = case sysloc of
                            "en" -> addressFileEn
                            oth  -> localesDirectory </> (unpack oth <> ".yml")

addressFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
addressFile fname = do
  exist <- liftIO $ doesFileExist fname
  if exist
  then pure fname
  else throwM $ InvalidLocale fname

fetchData :: (MonadThrow m, MonadIO m) => FakerSettings -> (Value -> Parser a) -> m a
fetchData settings parser = do
  let fname = guessAddressFile (fakerLocale settings)
  afile <- addressFile fname
  yaml <- decodeFileThrow afile
  parseMonad parser yaml

countries :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countries settings = fetchData settings parseCountry

newtype Unresolved a = Unresolved { unresolvedField :: a } deriving (Functor)

instance Applicative Unresolved where
    pure = Unresolved
    Unresolved f1 <*> Unresolved f2 = pure $ f1 f2
instance Monad Unresolved where
    return = pure
    (Unresolved f) >>= f1 = f1 f

parseBuildingNumber :: Value -> Parser (Unresolved (Vector Text))
parseBuildingNumber (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  address <- faker .: "address"
  building_number <- address .: "building_number"
  pure $ pure $ building_number
parseBuildingNumber val = fail $ "expected Object, but got " <> (show val)

parseAddress :: Object -> Parser Object
parseAddress obj = do
  en <- obj .: "en"
  faker <- en .: "faker"
  (Object address) <- faker .: "address"
  pure address

parseCommunityPrefix :: Value -> Parser (Vector Text)
parseCommunityPrefix (Object obj) = do
  address <- parseAddress obj
  community_prefix <- address .: "community_prefix"
  pure community_prefix
parseCommunityPrefix val = fail $ "expected Object, but got " <> (show val)

parseCommunitySuffix :: Value -> Parser (Vector Text)
parseCommunitySuffix (Object obj) = do
  address <- parseAddress obj
  community_suffix <- address .: "community_suffix"
  pure community_suffix
parseCommunitySuffix val = fail $ "expected Object, but got " <> (show val)

communitySuffix :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communitySuffix settings = fetchData settings parseCommunitySuffix

communityPrefix :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communityPrefix settings = fetchData settings parseCommunityPrefix

parseCommunity :: Value -> Parser (Unresolved (Vector Text))
parseCommunity (Object obj) = do
  address <- parseAddress obj
  community <- address .: "community"
  pure $ pure community
parseCommunity val = fail $ "expected Object, but got " <> (show val)

resolveCommunityField :: (MonadThrow m) => FakerSettings -> Text -> m Text
resolveCommunityField settings "community_suffix" = undefined --pick one from communitySuffix
resolveCommunityField settings "community_prefix" = undefined --pick one from communityPrefix
resolveCommunityField settings str = throwM $ InvalidField "community" str


resolveCommunityText :: Text -> IO Text
resolveCommunityText txt = undefined -- interoperate based on `txt`. Need to use `resolveCommunityField` function.
    where
      fields = resolveFields txt

extractSingleField :: Text -> (Text, Text)
extractSingleField txt = let (field, remaining) = T.span (\x -> x /= '}') txt''
                         in (T.drop 2 field, T.drop 1 remaining)
    where
      txt' = strip txt
      txt'' = snd $ T.span (\x -> x /= '#') txt'

resolveFields :: Text -> [Text]
resolveFields txt = let (field, remaining) = extractSingleField txt
                    in case T.null remaining of
                         True -> [field]
                         False -> [field] <> resolveFields remaining

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

randomIntText :: IO Text -> Char -> IO Text
randomIntText acc c = if isHash c
                      then do
                        gen <- newStdGen
                        a <- acc
                        let (int :: Int, _) = randomR (0,9) gen
                        pure $ a <> T.singleton (digitToChar int)
                      else do
                        a <- acc
                        pure $ a <> T.singleton c

interpolateNumbers :: Text -> IO Text
interpolateNumbers txt = T.foldl' randomIntText (pure T.empty) txt



interpolateAddress :: (MonadThrow m, MonadIO m) => Text -> m Text
interpolateAddress = undefined -- you have to use random function here

cities :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
cities settings = undefined

-- https://hackage.haskell.org/package/fake-0.1.1.1/docs/Fake.html
-- FGen type has to be modified to take a Config { cfLocale :: String, cfStdGen :: Gen}
