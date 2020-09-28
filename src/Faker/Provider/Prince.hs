{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.Prince where

import Config
import Control.Monad.Catch
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Monoid ((<>))
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parsePrince :: FromJSON a => FakerSettings -> Value -> Parser a
parsePrince settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  prince <- faker .: "prince"
  pure prince
parsePrince settings val = fail $ "expected Object, but got " <> (show val)

parsePrinceField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parsePrinceField settings txt val = do
  prince <- parsePrince settings val
  field <- prince .:? txt .!= mempty
  pure field

parsePrinceFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parsePrinceFields settings txts val = do
  prince <- parsePrince settings val
  helper prince txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)




parseUnresolvedPrinceFields ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> [Text]
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedPrinceFields settings txts val = do
  prince <- parsePrince settings val
  helper prince txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser (Unresolved a)
    helper a [] = do
      v <- parseJSON a
      pure $ pure v
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a _ = fail $ "expect Object, but got " <> (show a)





$(genParser "prince" "lyric")

$(genProvider "prince" "lyric")


$(genParser "prince" "song")

$(genProvider "prince" "song")


$(genParser "prince" "album")

$(genProvider "prince" "album")


$(genParser "prince" "band")

$(genProvider "prince" "band")











