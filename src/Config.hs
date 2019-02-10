{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Config where

import Control.Monad (filterM)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Yaml
import Faker
import Language.Haskell.TH (Name)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeExtension, takeFileName)
import System.FilePath

defaultEnDirectory :: FilePath
defaultEnDirectory = "faker/lib/locales/en"

localesDirectory :: FilePath
localesDirectory = "faker/lib/locales"

localesEnDirectory :: FilePath
localesEnDirectory = "faker/lib/locales/en"

isLocaleFile :: FilePath -> IO Bool
isLocaleFile fname = do
  exist <- doesFileExist fname
  pure (exist && (takeExtension fname == ".yml"))

listLocaleFiles :: FilePath -> IO [FilePath]
listLocaleFiles fname = do
  files <- listDirectory fname
  filterM isLocaleFile files

populateLocales :: IO [Text]
populateLocales = do
  files <- listLocaleFiles localesDirectory
  let files' = map (pack . takeFileName) files
  pure files'

data SourceData
  = Address
  | Name
  | Ancient
  | Animal
  | App
  | Appliance
  | ATHF
  | Artist
  | BTTF
  | Bank
  | Beer
  | BoJackHorseman
  | Book
  | BossaNova
  | BreakingBad
  | Buffy
  | Business
  | Cannabis
  | Cat
  | ChuckNorris
  | Code
  | Coffee
  | Coin
  | Color
  | Commerce
  | Community
  | Company
  | Construction
  | Cosmere
  | CryptoCoin
  | CultureSeries
  | Currency
  | DcComics
  | Demographic

sourceFile :: SourceData -> FilePath
sourceFile Address = "address"
sourceFile Name = "name"
sourceFile Ancient = "ancient"
sourceFile Animal = "animal"
sourceFile App = "app"
sourceFile Appliance = "appliance"
sourceFile ATHF = "aqua_teen_hunger_force"
sourceFile Artist = "artist"
sourceFile BTTF = "back_to_the_future"
sourceFile Bank = "bank"
sourceFile Beer = "beer"
sourceFile BoJackHorseman = "bojack_horseman"
sourceFile Book = "book"
sourceFile BossaNova = "bossa_nova"
sourceFile BreakingBad = "breaking_bad"
sourceFile Buffy = "buffy"
sourceFile Business = "business"
sourceFile Cannabis = "cannabis"
sourceFile Cat = "cat"
sourceFile ChuckNorris = "chuck_norris"
sourceFile Code = "code"
sourceFile Coffee = "coffee"
sourceFile Coin = "coin"
sourceFile Color = "color"
sourceFile Commerce = "commerce"
sourceFile Community = "community"
sourceFile Company = "company"
sourceFile Construction = "construction"
sourceFile Cosmere = "cosmere"
sourceFile CryptoCoin = "crypto_coin"
sourceFile CultureSeries = "culture_series"
sourceFile Currency = "currency"
sourceFile DcComics = "dc_comics"
sourceFile Demographic = "demographic"

mapSource :: Text -> Name
mapSource "address" = 'Address
mapSource "name" = 'Name
mapSource "ancient" = 'Ancient
mapSource "animal" = 'Animal
mapSource "app" = 'App
mapSource "appliance" = 'Appliance
mapSource "aquaTeenHungerForce" = 'ATHF
mapSource "artist" = 'Artist
mapSource "backToTheFuture" = 'BTTF
mapSource "bank" = 'Bank
mapSource "beer" = 'Beer
mapSource "bojackHourseman" = 'BoJackHorseman
mapSource "book" = 'Book
mapSource "bossaNova" = 'BossaNova
mapSource "breakingBad" = 'BreakingBad
mapSource "buffy" = 'Buffy
mapSource "business" = 'Business
mapSource "cannabis" = 'Cannabis
mapSource "cat" = 'Cat
mapSource "chuckNorris" = 'ChuckNorris
mapSource "code" = 'Code
mapSource "coffee" = 'Coffee
mapSource "coin" = 'Coin
mapSource "color" = 'Color
mapSource "commerce" = 'Commerce
mapSource "community" = 'Community
mapSource "company" = 'Company
mapSource "construction" = 'Construction
mapSource "cosmere" = 'Cosmere
mapSource "cryptoCoin" = 'CryptoCoin
mapSource "cultureSeries" = 'CultureSeries
mapSource "currency" = 'Currency
mapSource "dcComics" = 'DcComics
mapSource "demographic" = 'Demographic

guessSourceFile :: SourceData -> Text -> FilePath
guessSourceFile sdata sysloc =
  case sysloc of
    "en" -> localesEnDirectory </> (sourceFile sdata) <.> "yml"
    oth -> localesDirectory </> (unpack oth <> ".yml")

getSourceFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
getSourceFile fname = do
  exist <- liftIO $ doesFileExist fname
  if exist
    then pure fname
    else throwM $ InvalidLocale fname

fetchData ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> SourceData
  -> (FakerSettings -> Value -> Parser a)
  -> m a
fetchData settings sdata parser = do
  let fname = guessSourceFile sdata (getLocale settings)
  afile <- getSourceFile fname
  yaml <- decodeFileThrow afile
  parseMonad (parser settings) yaml
