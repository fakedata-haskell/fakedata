{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Faker.Creature.Bird where

import Data.Text (Text)
import Faker (Fake(..))
import Faker.Provider.Bird
import Faker.TH

$(generateFakeField "bird" "anatomy")

$(generateFakeField "bird" "anatomy_past_tense")

$(generateFakeField "bird" "geo")

$(generateFakeField "bird" "colors")

$(generateFakeField "bird" "emotional_adjectives")

$(generateFakeField "bird" "silly_adjectives")

$(generateFakeField "bird" "adjectives")

$(generateFakeField "bird" "common_family_name")


$(generateFakeFieldUnresolved "bird" "plausible_common_names")

$(generateFakeFieldUnresolved "bird" "implausible_common_names")


$(generateFakeFields "bird" ["order_common_map","Accipitriformes"])

$(generateFakeFields "bird" ["order_common_map","Anseriformes"])

$(generateFakeFields "bird" ["order_common_map","Apterygiformes"])

$(generateFakeFields "bird" ["order_common_map","Bucerotiformes"])

$(generateFakeFields "bird" ["order_common_map","Caprimulgiformes"])

$(generateFakeFields "bird" ["order_common_map","Cariamiformes"])

$(generateFakeFields "bird" ["order_common_map","Casuariiformes"])

$(generateFakeFields "bird" ["order_common_map","Cathartiformes"])

$(generateFakeFields "bird" ["order_common_map","Charadriiformes"])

$(generateFakeFields "bird" ["order_common_map","Ciconiiformes"])

$(generateFakeFields "bird" ["order_common_map","Coliiformes"])

$(generateFakeFields "bird" ["order_common_map","Columbiformes"])

$(generateFakeFields "bird" ["order_common_map","Coraciiformes"])

$(generateFakeFields "bird" ["order_common_map","Cuculiformes"])

$(generateFakeFields "bird" ["order_common_map","Eurypygiformes"])

$(generateFakeFields "bird" ["order_common_map","Falconiformes"])

$(generateFakeFields "bird" ["order_common_map","Galbuliformes"])

$(generateFakeFields "bird" ["order_common_map","Galliformes"])

$(generateFakeFields "bird" ["order_common_map","Gaviiformes"])

$(generateFakeFields "bird" ["order_common_map","Gruiformes"])

$(generateFakeFields "bird" ["order_common_map","Mesitornithiformes"])

$(generateFakeFields "bird" ["order_common_map","Musophagiformes"])

$(generateFakeFields "bird" ["order_common_map","Opisthocomiformes"])

$(generateFakeFields "bird" ["order_common_map","Otidiformes"])

$(generateFakeFields "bird" ["order_common_map","Passeriformes"])

$(generateFakeFields "bird" ["order_common_map","Pelecaniformes"])

$(generateFakeFields "bird" ["order_common_map","Phaethontiformes"])

$(generateFakeFields "bird" ["order_common_map","Phoenicopteriformes"])

$(generateFakeFields "bird" ["order_common_map","Piciformes"])

$(generateFakeFields "bird" ["order_common_map","Podicipediformes"])

$(generateFakeFields "bird" ["order_common_map","Procellariiformes"])

$(generateFakeFields "bird" ["order_common_map","Psittaciformes"])

$(generateFakeFields "bird" ["order_common_map","Pterocliformes"])

$(generateFakeFields "bird" ["order_common_map","Rheiformes"])

$(generateFakeFields "bird" ["order_common_map","Sphenisciformes"])

$(generateFakeFields "bird" ["order_common_map","Strigiformes"])

$(generateFakeFields "bird" ["order_common_map","Struthioniformes"])

$(generateFakeFields "bird" ["order_common_map","Suliformes"])

$(generateFakeFields "bird" ["order_common_map","Tinamiformes"])

$(generateFakeFields "bird" ["order_common_map","Trogoniformes"])
