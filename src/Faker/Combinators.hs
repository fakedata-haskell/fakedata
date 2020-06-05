module Faker.Combinators where

import Control.Monad
import Data.Foldable
import Data.List (sort)
import Faker
import System.Random

-- | Generates a random element in the given inclusive range.
fromRange :: Random a => (a, a) -> Fake a
fromRange rng =
  Fake
    (\r ->
       let (x, _) = randomR rng (getRandomGen r)
        in pure x)

-- | Generates a random element over the natural range of `a`.
pickAny :: Random a => Fake a
pickAny =
  Fake
    (\settings ->
       let (x, _) = random (getRandomGen settings)
        in pure x)

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Fake a -> (a -> Bool) -> Fake (Maybe a)
gen `suchThatMaybe` p = do
  x <- gen
  return $
    if p x
      then Just x
      else Nothing

-- | Generates a value that satisfies a predicate.
--
-- @
-- λ> import qualified Faker.Address as AD
-- λ> item :: Text <- generate $ suchThat AD.country (\x -> (T.length x > 5))
-- λ> item
-- "Ecuador"
-- @
--
suchThat :: Fake a -> (a -> Bool) -> Fake a
gen `suchThat` p = do
  mx <- gen `suchThatMaybe` p
  case mx of
    Just x -> return x
    Nothing -> gen `suchThat` p

-- | Randomly uses one of the given generators. The input structure
-- must be non-empty.
--
-- @
-- λ> import qualified Faker.Address as FA
-- λ> let fakes = [FA.country, FA.postcode, FA.state]
-- λ> generate (oneof fakes)
-- Montana
-- @
--
oneof :: Foldable t => t (Fake a) -> Fake a
oneof xs = helper
  where
    items = toList xs
    helper =
      case items of
        [] -> error "Faker.Combinators.oneof should be non-empty"
        xs' -> fromRange (0, length xs' - 1) >>= (items !!)

-- | Generates one of the given values. The input list must be non-empty.
--
-- @
-- λ> let fakeInt = elements [1..100]
-- λ> generate fakeInt
-- 22
-- @
--
elements :: Foldable t => t a -> Fake a
elements xs =
  case items of
    [] -> error "Faker.Combinators.element used with empty list"
    ys -> (ys !!) `fmap` fromRange (0, length xs - 1)
  where
    items = toList xs

-- | Generates a list of the given length.
listOf :: Int -> Fake a -> Fake [a]
listOf = replicateM

-- | Generates an ordered list.
orderedList :: (Ord a) => Int -> Fake a -> Fake [a]
orderedList n gen = sort <$> listOf n gen

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: [(Int, Fake a)] -> Fake a
frequency [] = error "Faker.Combinators.frequency used with empty list"
frequency xs0 = fromRange (1, tot) >>= (`pick` xs0)
  where
    tot = sum (map fst xs0)
    pick n ((k, x):xs)
      | n <= k = x
      | otherwise = pick (n - k) xs
    pick _ _ = error "Fake.pick used with empty list"

-- | Generate a value of an enumeration in the range [from, to].
-- @since 0.2.0
--
-- @
-- λ> data Animal = Cat | Dog | Zebra | Elephant | Giarfee deriving (Eq,Ord,Enum, Show)
-- λ> generate (fakeEnumFromTo Cat Zebra)
-- Zebra
-- @
--
fakeEnumFromTo :: Enum a => a -> a -> Fake a
fakeEnumFromTo from to = toEnum <$> fromRange (fromEnum from, fromEnum to)
