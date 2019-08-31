{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Nation where

import qualified Data.ByteString as BS
import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Faker
import Faker.Internal
import Faker.Provider.Nation
import Faker.TH

$(generateFakeField "nation" "nationality")

$(generateFakeField "nation" "language")

$(generateFakeField "nation" "capital_city")
-- flagEmoji :: Fake Text
-- flagEmoji =
--   Fake
--     (\settings -> do
--        vecword <- randomVec settings nationFlagEmojiProvider
--        let lword = V.toList vecword
--        pure $ TE.decodeUtf8 $ BS.pack lword)
