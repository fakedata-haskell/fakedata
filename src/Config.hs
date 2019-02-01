{-#LANGUAGE OverloadedStrings#-}

module Config where

import System.Directory (listDirectory, doesFileExist)
import Control.Monad (filterM)
import Data.Text (Text, pack, unpack)
import System.FilePath (takeFileName, takeExtension)
import Data.Yaml
import Control.Monad.IO.Class
import Control.Monad.Catch
import System.FilePath
import Faker

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

data SourceData = Address | Name

sourceFile :: SourceData -> FilePath
sourceFile Address = localesEnDirectory </> "address.yml"
sourceFile Name = localesEnDirectory </> "name.yml"

guessSourceFile :: SourceData -> Text -> FilePath
guessSourceFile sdata sysloc = case sysloc of
                                 "en" -> sourceFile sdata
                                 oth -> localesDirectory </> (unpack oth <> ".yml")

getSourceFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
getSourceFile fname = do
  exist <- liftIO $ doesFileExist fname
  if exist
  then pure fname
  else throwM $ InvalidLocale fname

fetchData :: (MonadThrow m, MonadIO m) => FakerSettings -> SourceData -> (Value -> Parser a) -> m a
fetchData settings sdata parser = do
  let fname = guessSourceFile sdata (fakerLocale settings)
  afile <- getSourceFile fname
  yaml <- decodeFileThrow afile
  parseMonad parser yaml
