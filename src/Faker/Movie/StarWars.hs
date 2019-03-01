{-# LANGUAGE TemplateHaskell #-}

module Faker.Movie.StarWars where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.StarWars
import Faker.TH

$(generateFakeField "starWars" "characters")

$(generateFakeField "starWars" "call_squadrons")

$(generateFakeField "starWars" "droids")

$(generateFakeField "starWars" "planets")

$(generateFakeField "starWars" "species")

$(generateFakeField "starWars" "vehicles")

$(generateFakeField "starWars" "wookiee_words")

$(generateFakeFieldUnresolved "starWars" "call_numbers")

$(generateFakeFieldUnresolved "starWars" "call_sign")

$(generateFakeFields "starWars" ["quotes", "admiral_ackbar"])

$(generateFakeFields "starWars" ["quotes", "ahsoka_tano"])

$(generateFakeFields "starWars" ["quotes", "anakin_skywalker"])

$(generateFakeFields "starWars" ["quotes", "asajj_ventress"])

$(generateFakeFields "starWars" ["quotes", "bendu"])

$(generateFakeFields "starWars" ["quotes", "boba_fett"])

$(generateFakeFields "starWars" ["quotes", "c_3po"])

$(generateFakeFields "starWars" ["quotes", "count_dooku"])

$(generateFakeFields "starWars" ["quotes", "darth_caedus"])

$(generateFakeFields "starWars" ["quotes", "darth_vader"])

$(generateFakeFields "starWars" ["quotes", "emperor_palpatine"])

$(generateFakeFields "starWars" ["quotes", "finn"])

$(generateFakeFields "starWars" ["quotes", "grand_admiral_thrawn"])

$(generateFakeFields "starWars" ["quotes", "grand_moff_tarkin"])

$(generateFakeFields "starWars" ["quotes", "greedo"])

$(generateFakeFields "starWars" ["quotes", "han_solo"])

$(generateFakeFields "starWars" ["quotes", "jabba_the_hutt"])

$(generateFakeFields "starWars" ["quotes", "jar_jar_binks"])

$(generateFakeFields "starWars" ["quotes", "k_2so"])

$(generateFakeFields "starWars" ["quotes", "kylo_ren"])

$(generateFakeFields "starWars" ["quotes", "lando_calrissian"])

$(generateFakeFields "starWars" ["quotes", "leia_organa"])

$(generateFakeFields "starWars" ["quotes", "luke_skywalker"])

$(generateFakeFields "starWars" ["quotes", "mace_windu"])

$(generateFakeFields "starWars" ["quotes", "maz_kanata"])

$(generateFakeFields "starWars" ["quotes", "obi_wan_kenobi"])

$(generateFakeFields "starWars" ["quotes", "padme_amidala"])

$(generateFakeFields "starWars" ["quotes", "qui_gon_jinn"])

$(generateFakeFields "starWars" ["quotes", "rey"])

$(generateFakeFields "starWars" ["quotes", "shmi_skywalker"])

$(generateFakeFields "starWars" ["quotes", "yoda"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "admiral_ackbar"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "ahsoka_tano"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "anakin_skywalker"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "asajj_ventress"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "bendu"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "boba_fett"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "c_3po"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "count_dooku"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "darth_caedus"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "darth_vader"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "emperor_palpatine"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "finn"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "general_hux"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "grand_admiral_thrawn"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "grand_moff_tarkin"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "greedo"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "han_solo"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "jabba_the_hutt"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "jar_jar_binks"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "k_2so"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "kylo_ren"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "lando_calrissian"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "leia_organa"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "luke_skywalker"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "mace_windu"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "maz_kanata"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "obi_wan_kenobi"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "padme_amidala"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "qui_gon_jinn"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "rey"])

$(generateFakeFields
    "starWars"
    ["alternate_character_spellings", "shmi_skywalker"])

$(generateFakeFields "starWars" ["alternate_character_spellings", "yoda"])
