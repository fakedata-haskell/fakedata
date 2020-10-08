{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Data.Text
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
       let item :: V.Vector (IO Text) = V.map (\v -> rvec settings v) vec
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
  pure $ fixupName companyName <> "." <> suffix

-- | Generates an email like "jappie_klooster@crazychairauction.com"
--
-- @since 0.8.1
--
email :: Fake Text
email = do
  humanName <-  F.name
  number <- F.fromRange @Int (0, 999_999_999) -- reasonable uniqueness
  domainName <- domain
  let numText :: Text
      numText = pack $ show number
  pure $ fixupName humanName <> "-" <> numText <> "@" <> domainName

-- | Ensures the spaces are replaced by "_",
--   and no special characters are in the name.
--   So "Elizabeth Warder!" becomes "Elizabeth_Warder".
--   Any fancy symbols such as "!@#$" etc are filtered out.
fixupName :: Text -> Text
fixupName = Data.Text.filter (\c -> isAlphaNum c || c == '_') . replace " " "_"
