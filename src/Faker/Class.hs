module Faker.Class where

import Faker
import Faker.Combinators
import Control.Monad

class Faker a where
    faker :: Fake a

instance Faker () where
    faker = return ()

instance Faker Bool where
    faker = fromRange (True, False)

instance Faker Ordering where
    faker = elements [EQ, LT, GT]

instance Faker a => Faker (Maybe a) where
    faker = frequency [(1, return Nothing), (3, liftM Just faker)]

instance (Faker a, Faker b) => Faker (Either a b) where
    faker = oneof [liftM Left faker, liftM Right faker]

instance (Faker a, Faker b) => Faker (a,b) where
    faker = liftM2 (,) faker faker

instance (Faker a, Faker b, Faker c) => Faker (a,b,c) where
    faker = liftM3 (,,) faker faker faker

instance (Faker a, Faker b, Faker c, Faker d) => Faker (a,b,c,d) where
    faker = liftM4 (,,,) faker faker faker faker

instance (Faker a, Faker b, Faker c, Faker d, Faker e) => Faker (a,b,c,d,e) where
    faker = liftM5 (,,,,) faker faker faker faker faker

instance (Faker a, Faker b, Faker c, Faker d, Faker e, Faker f)
      => Faker (a,b,c,d,e,f) where
    faker = return (,,,,,)
          <*> faker <*> faker <*> faker <*> faker <*> faker <*> faker

instance (Faker a, Faker b, Faker c, Faker d, Faker e, Faker f, Faker g)
      => Faker (a,b,c,d,e,f,g) where
    faker = return (,,,,,,)
          <*> faker <*> faker <*> faker <*> faker <*> faker <*> faker <*> faker
