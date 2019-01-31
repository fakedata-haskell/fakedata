{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
{-#LANGUAGE BangPatterns#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Faker.Address where

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
import Debug.Trace

parseAddress :: FromJSON a => Value -> Parser a
parseAddress (Object obj) = do
  en <- obj .: "en"
  faker <- en .: "faker"
  address <- faker .: "address"
  pure address
parseAddress val = fail $ "expected Object, but got " <> (show val)

parseAddressField :: FromJSON a => Text -> Value -> Parser a
parseAddressField txt val = do
  address <- parseAddress val
  field <- address .: txt
  pure field

parseUnresolvedAddressField :: FromJSON a => Text -> Value -> Parser (Unresolved a)
parseUnresolvedAddressField txt val = do
  address <- parseAddress val
  field <- address .: txt
  pure $ pure field

parseCityPrefix :: FromJSON a => Value -> Parser a
parseCityPrefix val = do
  address <- parseAddress val
  cityPrefix <- address .: "city_prefix"
  pure cityPrefix

parseCitySuffix :: FromJSON a => Value -> Parser a
parseCitySuffix val = do
  address <- parseAddress val
  citySuffix <- address .: "city_suffix"
  pure citySuffix

parseCountry :: FromJSON a => Value -> Parser a
parseCountry val = do
  address <- parseAddress val
  country <- address .: "country"
  pure country

parseCountryByCode :: FromJSON a => Value -> Parser a
parseCountryByCode = parseAddressField "country_by_code"

parseCountryByName :: FromJSON a => Value -> Parser a
parseCountryByName = parseAddressField "country_by_name"

parseCountryCode :: FromJSON a => Value -> Parser a
parseCountryCode = parseAddressField "country_code"

parseCountryCodeLong :: FromJSON a => Value -> Parser a
parseCountryCodeLong = parseAddressField "country_code_long"

parseBuildingNumber :: FromJSON a => Value -> Parser (Unresolved a)
parseBuildingNumber = parseUnresolvedAddressField "building_number"

parseCommunityPrefix :: Value -> Parser (Vector Text)
parseCommunityPrefix val = do
  address <- parseAddress val
  community_prefix <- address .: "community_prefix"
  pure community_prefix

parseCommunitySuffix :: Value -> Parser (Vector Text)
parseCommunitySuffix val = do
  address <- parseAddress val
  community_suffix <- address .: "community_suffix"
  pure community_suffix

parseCommunity :: Value -> Parser (Unresolved (Vector Text))
parseCommunity val = do
  address <- parseAddress val
  community <- address .: "community"
  pure $ pure community

parseSecondaryAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseSecondaryAddress = parseUnresolvedAddressField "secondary_address"

parsePostcode :: FromJSON a => Value -> Parser (Unresolved a)
parsePostcode = parseUnresolvedAddressField "postcode"

parsePostcodeByState :: FromJSON a => Value -> Parser (Unresolved a)
parsePostcodeByState = parseUnresolvedAddressField "postcode_by_state"

parseState :: FromJSON a => Value -> Parser a
parseState = parseAddressField "state"

parseStateAbbr :: FromJSON a => Value -> Parser a
parseStateAbbr = parseAddressField "state_abbr"

parseTimeZone :: FromJSON a => Value -> Parser a
parseTimeZone = parseAddressField "time_zone"

parseCity :: FromJSON a => Value -> Parser (Unresolved a)
parseCity = parseUnresolvedAddressField "city"

parseStreetName :: FromJSON a => Value -> Parser (Unresolved a)
parseStreetName = parseUnresolvedAddressField "street_name"

parseStreetAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseStreetAddress = parseUnresolvedAddressField "street_address"

parseFullAddress :: FromJSON a => Value -> Parser (Unresolved a)
parseFullAddress = parseUnresolvedAddressField "full_address"

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

newtype Unresolved a = Unresolved { unresolvedField :: a } deriving (Functor)

instance Applicative Unresolved where
    pure = Unresolved
    Unresolved f1 <*> Unresolved f2 = pure $ f1 f2
instance Monad Unresolved where
    return = pure
    (Unresolved f) >>= f1 = f1 f

fetchData :: (MonadThrow m, MonadIO m) => FakerSettings -> (Value -> Parser a) -> m a
fetchData settings parser = do
  let fname = guessAddressFile (fakerLocale settings)
  afile <- addressFile fname
  yaml <- decodeFileThrow afile
  parseMonad parser yaml

countriesProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countriesProvider settings = fetchData settings parseCountry

cityPrefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
cityPrefixProvider settings = fetchData settings parseCityPrefix

citySuffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
citySuffixProvider settings = fetchData settings parseCitySuffix

countryByCodeProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByCodeProvider settings = fetchData settings parseCountryByCode

countryByNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Map Text Text)
countryByNameProvider settings = fetchData settings parseCountryByName

countryCodeProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeProvider settings = fetchData settings parseCountryCode

countryCodeLongProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
countryCodeLongProvider settings = fetchData settings parseCountryCodeLong

buildingNumberProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
buildingNumberProvider settings = fetchData settings parseBuildingNumber

communityPrefixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communityPrefixProvider settings = fetchData settings parseCommunityPrefix

communitySuffixProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text)
communitySuffixProvider settings = fetchData settings parseCommunitySuffix

communityProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
communityProvider settings = fetchData settings parseCommunity

resolveUnresolved :: (MonadThrow m, MonadIO m) => FakerSettings -> Unresolved (Vector Text) -> m Text
resolveUnresolved settings (Unresolved unres) =
    let unresLen = V.length unres
        (index, _) = randomR (0, unresLen) (getRandomGen settings)
        randomItem = unres ! index
        resolve = if operateField randomItem "hello" == randomItem
                  then resolveAddressField settings randomItem
                  else interpolateNumbers randomItem
    in resolve


-- todo: i don't think this is proper random. Running it multiple times will give the same result. check it.
resolveAddressField :: (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveAddressField settings "community_suffix" = do
  csuffix <- communitySuffixProvider settings
  let csuffixLen = V.length csuffix
      (index, _) = randomR (0, csuffixLen) (getRandomGen settings)
  pure $ csuffix ! index
resolveAddressField settings "community_prefix" = do
  cprefix <- communityPrefixProvider settings
  let cprefixLen = V.length cprefix
      (index, _) = randomR (0, cprefixLen) (getRandomGen settings)
  pure $ cprefix ! index
resolveAddressField settings str = throwM $ InvalidField "community" str

uncons2 :: Text -> Maybe (String, Text)
uncons2 txt = do
   (c1, rem1) <- T.uncons txt
   (c2, rem2) <- T.uncons rem1
   pure $ ((c1:[c2]), rem2)

-- operateField "#{hello} #{world}" "jam"
-- "jam #{world}"
operateField :: Text -> Text -> Text
operateField origWord word = helper origWord word []
  where
    helper :: Text -> Text -> String -> Text
    helper txt word acc =
      case uncons2 txt of
        Nothing -> origWord
        Just (str, rem) ->
          if str == "#{"
            then let actualRem = dropTillBrace rem
                  in (T.pack acc) <> word <> actualRem
            else case T.uncons txt of
                   Nothing -> origWord
                   Just (c, rem2) -> helper rem2 word (acc <> [c])

operateFields :: Text -> [Text] -> Text
operateFields origWord [] = origWord
operateFields origWord (x:xs) = operateFields (operateField origWord x) xs

dropTillBrace :: Text -> Text
dropTillBrace txt = T.dropWhile (== '}') $ T.dropWhile (/= '}') txt

-- > resolveCommunityText "#{community_prefix} #{community_suffix}"
resolveCommunityText :: FakerSettings -> Text -> IO Text
resolveCommunityText settings txt = do
    communityFields :: [Text] <- mapM (resolveAddressField settings) fields
    pure $ operateFields txt communityFields
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

randomIntText :: (MonadIO m) => m Text -> Char -> m Text
randomIntText acc c = if isHash c
                      then do
                        gen <- liftIO $ newStdGen
                        a <- acc
                        let (int :: Int, _) = randomR (0,9) gen
                        pure $ a <> T.singleton (digitToChar int)
                      else do
                        a <- acc
                        pure $ a <> T.singleton c

-- >> interpolateNumbers "#####"
-- >> 23456
-- >> interpolateNumbers "ab-##"
-- >> ab-32
interpolateNumbers :: (MonadIO m) => Text -> m Text
interpolateNumbers txt = T.foldl' randomIntText (pure T.empty) txt



interpolateAddress :: (MonadThrow m, MonadIO m) => Text -> m Text
interpolateAddress = undefined -- you have to use random function here

cities :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text))
cities settings = undefined

-- https://hackage.haskell.org/package/fake-0.1.1.1/docs/Fake.html
-- FGen type has to be modified to take a Config { cfLocale :: String, cfStdGen :: Gen}
