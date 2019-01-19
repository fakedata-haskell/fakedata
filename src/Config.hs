module Config where

import System.Directory (listDirectory, doesFileExist)
import Control.Monad (filterM)
import Data.Text (Text, pack)
import System.FilePath (takeFileName, takeExtension)

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
