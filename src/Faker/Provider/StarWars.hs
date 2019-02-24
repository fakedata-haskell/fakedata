{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Faker.Provider.StarWars where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Faker.Provider.TH
import Language.Haskell.TH

parseStarWars :: FromJSON a => FakerSettings -> Value -> Parser a
parseStarWars settings (Object obj) = do
  en <- obj .: (getLocale settings)
  faker <- en .: "faker"
  starWars <- faker .: "star_wars"
  pure starWars
parseStarWars settings val = fail $ "expected Object, but got " <> (show val)

parseStarWarsField ::
     (FromJSON a, Monoid a) => FakerSettings -> Text -> Value -> Parser a
parseStarWarsField settings txt val = do
  starWars <- parseStarWars settings val
  field <- starWars .:? txt .!= mempty
  pure field

parseStarWarsFields ::
     (FromJSON a, Monoid a) => FakerSettings -> [Text] -> Value -> Parser a
parseStarWarsFields settings txts val = do
  starWars <- parseStarWars settings val
  helper starWars txts
  where
    helper :: (FromJSON a) => Value -> [Text] -> Parser a
    helper a [] = parseJSON a
    helper (Object a) (x:xs) = do
      field <- a .: x
      helper field xs
    helper a (x:xs) = fail $ "expect Object, but got " <> (show a)

parseUnresolvedStarWarsField ::
     (FromJSON a, Monoid a)
  => FakerSettings
  -> Text
  -> Value
  -> Parser (Unresolved a)
parseUnresolvedStarWarsField settings txt val = do
  starWars <- parseStarWars settings val
  field <- starWars .:? txt .!= mempty
  pure $ pure field

$(genParser "starWars" "characters")

$(genProvider "starWars" "characters")

$(genParser "starWars" "call_squadrons")

$(genProvider "starWars" "call_squadrons")

$(genParser "starWars" "droids")

$(genProvider "starWars" "droids")

$(genParser "starWars" "planets")

$(genProvider "starWars" "planets")

$(genParser "starWars" "species")

$(genProvider "starWars" "species")

$(genParser "starWars" "vehicles")

$(genProvider "starWars" "vehicles")

$(genParser "starWars" "wookiee_words")

$(genProvider "starWars" "wookiee_words")

$(genParserUnresolved "starWars" "call_numbers")

$(genProviderUnresolved "starWars" "call_numbers")

$(genParserUnresolved "starWars" "call_sign")

$(genProviderUnresolved "starWars" "call_sign")

resolveStarWarsText ::
     (MonadIO m, MonadThrow m) => FakerSettings -> Text -> m Text
resolveStarWarsText settings txt = do
  let fields = resolveFields txt
  starWarsFields <- mapM (resolveStarWarsField settings) fields
  pure $ operateFields txt starWarsFields

resolveStarWarsField ::
     (MonadThrow m, MonadIO m) => FakerSettings -> Text -> m Text
resolveStarWarsField settings "call_number" =
  randomUnresolvedVec settings starWarsCallNumbersProvider resolveStarWarsText
resolveStarWarsField settings "call_sign" =
  randomUnresolvedVec settings starWarsCallSignProvider resolveStarWarsText
resolveStarWarsField settings "call_squadron" =
  randomVec settings starWarsCallSquadronsProvider
resolveStarWarsField settings str = throwM $ InvalidField "starWars" str

$(genParsers "starWars" ["quotes", "admiral_ackbar"])

$(genProviders "starWars" ["quotes", "admiral_ackbar"])

$(genParsers "starWars" ["quotes", "ahsoka_tano"])

$(genProviders "starWars" ["quotes", "ahsoka_tano"])

$(genParsers "starWars" ["quotes", "anakin_skywalker"])

$(genProviders "starWars" ["quotes", "anakin_skywalker"])

$(genParsers "starWars" ["quotes", "asajj_ventress"])

$(genProviders "starWars" ["quotes", "asajj_ventress"])

$(genParsers "starWars" ["quotes", "bendu"])

$(genProviders "starWars" ["quotes", "bendu"])

$(genParsers "starWars" ["quotes", "boba_fett"])

$(genProviders "starWars" ["quotes", "boba_fett"])

$(genParsers "starWars" ["quotes", "c_3po"])

$(genProviders "starWars" ["quotes", "c_3po"])

$(genParsers "starWars" ["quotes", "count_dooku"])

$(genProviders "starWars" ["quotes", "count_dooku"])

$(genParsers "starWars" ["quotes", "darth_caedus"])

$(genProviders "starWars" ["quotes", "darth_caedus"])

$(genParsers "starWars" ["quotes", "darth_vader"])

$(genProviders "starWars" ["quotes", "darth_vader"])

$(genParsers "starWars" ["quotes", "emperor_palpatine"])

$(genProviders "starWars" ["quotes", "emperor_palpatine"])

$(genParsers "starWars" ["quotes", "finn"])

$(genProviders "starWars" ["quotes", "finn"])

$(genParsers "starWars" ["quotes", "grand_admiral_thrawn"])

$(genProviders "starWars" ["quotes", "grand_admiral_thrawn"])

$(genParsers "starWars" ["quotes", "grand_moff_tarkin"])

$(genProviders "starWars" ["quotes", "grand_moff_tarkin"])

$(genParsers "starWars" ["quotes", "greedo"])

$(genProviders "starWars" ["quotes", "greedo"])

$(genParsers "starWars" ["quotes", "han_solo"])

$(genProviders "starWars" ["quotes", "han_solo"])

$(genParsers "starWars" ["quotes", "jabba_the_hutt"])

$(genProviders "starWars" ["quotes", "jabba_the_hutt"])

$(genParsers "starWars" ["quotes", "jar_jar_binks"])

$(genProviders "starWars" ["quotes", "jar_jar_binks"])

$(genParsers "starWars" ["quotes", "k_2so"])

$(genProviders "starWars" ["quotes", "k_2so"])

$(genParsers "starWars" ["quotes", "kylo_ren"])

$(genProviders "starWars" ["quotes", "kylo_ren"])

$(genParsers "starWars" ["quotes", "lando_calrissian"])

$(genProviders "starWars" ["quotes", "lando_calrissian"])

$(genParsers "starWars" ["quotes", "leia_organa"])

$(genProviders "starWars" ["quotes", "leia_organa"])

$(genParsers "starWars" ["quotes", "luke_skywalker"])

$(genProviders "starWars" ["quotes", "luke_skywalker"])

$(genParsers "starWars" ["quotes", "mace_windu"])

$(genProviders "starWars" ["quotes", "mace_windu"])

$(genParsers "starWars" ["quotes", "maz_kanata"])

$(genProviders "starWars" ["quotes", "maz_kanata"])

$(genParsers "starWars" ["quotes", "obi_wan_kenobi"])

$(genProviders "starWars" ["quotes", "obi_wan_kenobi"])

$(genParsers "starWars" ["quotes", "padme_amidala"])

$(genProviders "starWars" ["quotes", "padme_amidala"])

$(genParsers "starWars" ["quotes", "qui_gon_jinn"])

$(genProviders "starWars" ["quotes", "qui_gon_jinn"])

$(genParsers "starWars" ["quotes", "rey"])

$(genProviders "starWars" ["quotes", "rey"])

$(genParsers "starWars" ["quotes", "shmi_skywalker"])

$(genProviders "starWars" ["quotes", "shmi_skywalker"])

$(genParsers "starWars" ["quotes", "yoda"])

$(genProviders "starWars" ["quotes", "yoda"])

$(genParsers "starWars" ["alternate_character_spellings", "admiral_ackbar"])

$(genProviders "starWars" ["alternate_character_spellings", "admiral_ackbar"])

$(genParsers "starWars" ["alternate_character_spellings", "ahsoka_tano"])

$(genProviders "starWars" ["alternate_character_spellings", "ahsoka_tano"])

$(genParsers "starWars" ["alternate_character_spellings", "anakin_skywalker"])

$(genProviders "starWars" ["alternate_character_spellings", "anakin_skywalker"])

$(genParsers "starWars" ["alternate_character_spellings", "asajj_ventress"])

$(genProviders "starWars" ["alternate_character_spellings", "asajj_ventress"])

$(genParsers "starWars" ["alternate_character_spellings", "bendu"])

$(genProviders "starWars" ["alternate_character_spellings", "bendu"])

$(genParsers "starWars" ["alternate_character_spellings", "boba_fett"])

$(genProviders "starWars" ["alternate_character_spellings", "boba_fett"])

$(genParsers "starWars" ["alternate_character_spellings", "c_3po"])

$(genProviders "starWars" ["alternate_character_spellings", "c_3po"])

$(genParsers "starWars" ["alternate_character_spellings", "count_dooku"])

$(genProviders "starWars" ["alternate_character_spellings", "count_dooku"])

$(genParsers "starWars" ["alternate_character_spellings", "darth_caedus"])

$(genProviders "starWars" ["alternate_character_spellings", "darth_caedus"])

$(genParsers "starWars" ["alternate_character_spellings", "darth_vader"])

$(genProviders "starWars" ["alternate_character_spellings", "darth_vader"])

$(genParsers "starWars" ["alternate_character_spellings", "emperor_palpatine"])

$(genProviders "starWars" ["alternate_character_spellings", "emperor_palpatine"])

$(genParsers "starWars" ["alternate_character_spellings", "finn"])

$(genProviders "starWars" ["alternate_character_spellings", "finn"])

$(genParsers "starWars" ["alternate_character_spellings", "general_hux"])

$(genProviders "starWars" ["alternate_character_spellings", "general_hux"])

$(genParsers
    "starWars"
    ["alternate_character_spellings", "grand_admiral_thrawn"])

$(genProviders
    "starWars"
    ["alternate_character_spellings", "grand_admiral_thrawn"])

$(genParsers "starWars" ["alternate_character_spellings", "grand_moff_tarkin"])

$(genProviders "starWars" ["alternate_character_spellings", "grand_moff_tarkin"])

$(genParsers "starWars" ["alternate_character_spellings", "greedo"])

$(genProviders "starWars" ["alternate_character_spellings", "greedo"])

$(genParsers "starWars" ["alternate_character_spellings", "han_solo"])

$(genProviders "starWars" ["alternate_character_spellings", "han_solo"])

$(genParsers "starWars" ["alternate_character_spellings", "jabba_the_hutt"])

$(genProviders "starWars" ["alternate_character_spellings", "jabba_the_hutt"])

$(genParsers "starWars" ["alternate_character_spellings", "jar_jar_binks"])

$(genProviders "starWars" ["alternate_character_spellings", "jar_jar_binks"])

$(genParsers "starWars" ["alternate_character_spellings", "k_2so"])

$(genProviders "starWars" ["alternate_character_spellings", "k_2so"])

$(genParsers "starWars" ["alternate_character_spellings", "kylo_ren"])

$(genProviders "starWars" ["alternate_character_spellings", "kylo_ren"])

$(genParsers "starWars" ["alternate_character_spellings", "lando_calrissian"])

$(genProviders "starWars" ["alternate_character_spellings", "lando_calrissian"])

$(genParsers "starWars" ["alternate_character_spellings", "leia_organa"])

$(genProviders "starWars" ["alternate_character_spellings", "leia_organa"])

$(genParsers "starWars" ["alternate_character_spellings", "luke_skywalker"])

$(genProviders "starWars" ["alternate_character_spellings", "luke_skywalker"])

$(genParsers "starWars" ["alternate_character_spellings", "mace_windu"])

$(genProviders "starWars" ["alternate_character_spellings", "mace_windu"])

$(genParsers "starWars" ["alternate_character_spellings", "maz_kanata"])

$(genProviders "starWars" ["alternate_character_spellings", "maz_kanata"])

$(genParsers "starWars" ["alternate_character_spellings", "obi_wan_kenobi"])

$(genProviders "starWars" ["alternate_character_spellings", "obi_wan_kenobi"])

$(genParsers "starWars" ["alternate_character_spellings", "padme_amidala"])

$(genProviders "starWars" ["alternate_character_spellings", "padme_amidala"])

$(genParsers "starWars" ["alternate_character_spellings", "qui_gon_jinn"])

$(genProviders "starWars" ["alternate_character_spellings", "qui_gon_jinn"])

$(genParsers "starWars" ["alternate_character_spellings", "rey"])

$(genProviders "starWars" ["alternate_character_spellings", "rey"])

$(genParsers "starWars" ["alternate_character_spellings", "shmi_skywalker"])

$(genProviders "starWars" ["alternate_character_spellings", "shmi_skywalker"])

$(genParsers "starWars" ["alternate_character_spellings", "yoda"])

$(genProviders "starWars" ["alternate_character_spellings", "yoda"])
