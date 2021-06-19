{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Music.Opera where

import Data.Text
import Faker
import Faker.Internal
import Faker.Provider.Opera
import Faker.TH

$(generateFakeFields "opera" ["italian", "by_giuseppe_verdi"])

$(generateFakeFields "opera" ["italian", "by_gioacchino_rossini"])

$(generateFakeFields "opera" ["italian", "by_gaetano_donizetti"])

$(generateFakeFields "opera" ["italian", "by_vincenzo_bellini"])

-- | @since 1.0
$(generateFakeFields "opera" ["italian", "by_christoph_willibald_gluck"])

-- | @since 1.0
$(generateFakeFields "opera" ["italian", "by_wolfgang_amadeus_mozart"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_wolfgang_amadeus_mozart"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_ludwig_van_beethoven"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_carl_maria_von_weber"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_richard_strauss"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_richard_wagner"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_robert_schumann"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_franz_schubert"])

-- | @since 1.0
$(generateFakeFields "opera" ["german", "by_alban_berg"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_christoph_willibald_gluck"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_maurice_ravel"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_hector_berlioz"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_georges_bizet"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_charles_gounod"])

-- | @since 1.0
$(generateFakeFields "opera" ["french", "by_camille_saint_saëns"])
