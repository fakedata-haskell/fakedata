{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Faker.Company
  ( buzzword
  , suffix
  , bs
  , name
  , industry
  , profession
  , type'
  , sicCode
  , email
  , domain
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Vector as V
import Faker
import Faker.Internal
import Faker.Provider.Company
import Faker.TH
import qualified Faker.Internet as F
import qualified Faker.Name as F
import qualified Faker.Combinators as F
import Data.Char(isAlphaNum)

$(generateFakeField "company" "suffix")

buzzword :: Fake Text
buzzword =
  Fake
    (\settings -> do
       vec :: V.Vector (V.Vector Text) <- companyBuzzwordsProvider settings
       let item :: IO (V.Vector Text) = rvec settings vec
       item' <- item
       rvec settings item')

bs :: Fake Text
bs =
  Fake
    (\settings -> do
       vec :: V.Vector (V.Vector Text) <- companyBsProvider settings
       let item :: V.Vector (IO Text) = V.map (rvec settings) vec
           item' :: IO (V.Vector Text) = sequence item
       items <- item'
       let txt = V.foldl1' (\a b -> a <> " " <> b) items
       pure txt)

$(generateFakeFieldUnresolved "company" "name")

$(generateFakeField "company" "industry")

$(generateFakeField "company" "profession")

$(generateFakeField "company" "type")

-- | SIC code for classifying industries.
--
-- @since 0.2.0
--
$(generateFakeField "company" "sic_code")

-- | Generates a domain name like "crazychairauction.com"
--
-- @since 0.8.1
--
domain :: Fake Text
domain = do
  suffix <- F.domainSuffix
  companyName <- name
  pure $ sanitise companyName <> "." <> suffix
  where
  -- Replaces spaces with hyphens and filters out anything that isn't an
  -- alphanumeric character or a hyphen, so the domain has a better chance of
  -- conforming to RFC 1035.
  --
  -- See: https://datatracker.ietf.org/doc/html/rfc1035#section-2.3.1
  sanitise :: Text -> Text
  sanitise = T.filter (\c -> isAlphaNum c || c == '-') . T.replace " " "-"

-- | Generates an email like "jappie_klooster@crazychairauction.com"
--
-- @since 0.8.1
email :: Fake Text
email = do
  humanName <-  F.name
  number <- F.fromRange @Int (0, 999999999) -- reasonable uniqueness
  domainName <- domain
  let numText :: Text
      numText = T.pack $ show number
  pure $ sanitise humanName <> "-" <> numText <> "@" <> domainName
  where
  -- Ensures the spaces are replaced by "_",
  -- and no special characters are in the name.
  -- So "Elizabeth Warder!" becomes "Elizabeth_Warder".
  -- Any fancy symbols such as "!@#$" etc are filtered out.
  sanitise :: Text -> Text
  sanitise = T.filter (\c -> isAlphaNum c || c == '_') . T.replace " " "_"
