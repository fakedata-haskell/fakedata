![fakedata](https://user-images.githubusercontent.com/737477/53658993-54575200-3c80-11e9-9125-fbcf9e54660f.png)
[![Hackage](https://img.shields.io/hackage/v/fakedata.svg)](https://hackage.haskell.org/package/fakedata)
[![Stackage
Nightly](http://stackage.org/package/fakedata/badge/nightly)](http://stackage.org/nightly/package/fakedata)
[![Stackage
LTS](http://stackage.org/package/fakedata/badge/lts)](http://stackage.org/lts/package/fakedata)
![Build Status](https://github.com/psibi/fakedata/workflows/Tests/badge.svg?branch=master)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [fakedata](#fakedata)
- [Tutorial](#tutorial)
  - [Generating address](#generating-address)
  - [Generating name](#generating-name)
  - [Generate quotes from the movie Back to the Future](#generate-quotes-from-the-movie-back-to-the-future)
  - [Combining Fake datas](#combining-fake-datas)
- [Deterministic vs Non Deterministic values](#deterministic-vs-non-deterministic-values)
- [Combinators](#combinators)
  - [listOf](#listof)
  - [oneOf](#oneof)
  - [suchThat](#suchthat)
- [Using the `FakeT` transformer](#using-the-faket-transformer)
- [Comparision with other libraries](#comparision-with-other-libraries)
- [Acknowledgments](#acknowledgments)

<!-- markdown-toc end -->


# fakedata

This library is a port of Ruby's [faker](https://github.com/stympy/faker). It's a library for
producing fake data such as names, addressess and phone numbers. Note
that it directly uses the source data from that library, so the
quality of fake data is quite high!

This package comes in handy when you have to generate large amount of
real like data for various purposes. I have personally used it for
websites where it needs some realistic data in the initial stage,
loading database with real like values etc. There are companies which
have used this [for sophisphicated testing purpose](https://www.reddit.com/r/haskell/comments/o5n71e/fakedata10_haskell_library_for_producing_quality/h2qxw8a?utm_source=share&utm_medium=web2x&context=3).

Additionly, there are two other packages for creating generators which
is useful for property testing:

* [fakedata-quickcheck](https://github.com/fakedata-haskell/fakedata-quickcheck)
* [hedgehog-fakedata](https://github.com/parsonsmatt/hedgehog-fakedata)

# Tutorial

## Generating address

``` haskell
~/g/fakedata (master) $ stack ghci
λ> import Faker
λ> import Faker.Address
λ> address <- generate fullAddress
λ> address
"Apt. 298 340 Ike Mission, Goldnertown, FL 19488-9259"
```

## Generating name

``` haskell
λ> fullName <- generate name
λ> fullName
"Sherryl Steuber"
```

## Generate quotes from the movie Back to the Future

``` haskell
λ> import Faker.Movie.BackToTheFuture
λ> import Faker.Combinators
λ> qs <- generateNonDeterministic $ listOf 5 quotes
λ> qs
[ "Yes. Yes. I'm George. George McFly. I'm your density. I mean, your destiny."
, "Hello? Hello? Anybody home? Huh? Think, McFly. Think! I gotta have time to get them retyped. Do you realize what would happen if I hand in my reports in your handwriting? I'll get fired. You wouldn't want that to happen, would ya? Would ya?"
, "Lorraine. My density has brought me to you."
, "See you in about 30 years."
, "You really think I ought to swear?"
]
```

## Combining Fake datas

``` haskell
{-#LANGUAGE RecordWildCards#-}

import Faker
import Faker.Name
import Faker.Address
import Data.Text

data Person = Person {
    personName :: Text,
    personAddress :: Text
} deriving (Show, Eq)

fakePerson :: Fake Person
fakePerson = do
    personName <- name
    personAddress <- fullAddress
    pure $ Person{..}

main :: IO ()
main = do
    person <- generate fakePerson
    print person
```

And on executing them:

``` haskell
$ stack name.hs
Person
  { personName = "Sherryl Steuber"
  , personAddress = "Apt. 298 340 Ike Mission, Goldnertown, FL 19488-9259"
  }
```

You would have noticed in the above output that the name and address are
the same as generated before in the GHCi REPL. That's because, by
default all the generated data are deterministic. If you want a
different set of output each time, you would have to modify the random
generator output:

``` haskell
main :: IO ()
main = do
    gen <- newStdGen
    let settings = setRandomGen gen defaultFakerSettings
    person <- generateWithSettings settings fakePerson
    print person
```

And on executing the program, you will get a different output:

``` haskell
Person
  { personName = "Ned Effertz Sr."
  , personAddress = "Suite 158 1580 Schulist Mall, Schulistburgh, NY 15804-3392"
  }
```

The above program can be even minimized like this:

``` haskell
main :: IO ()
main = do
    let settings = setNonDeterministic defaultFakerSettings
    person <- generateWithSettings settings fakePerson
    print person
```

Or even better:

``` haskell
main :: IO ()
main = do
    person <- generateNonDeterministic fakePerson
    print person
```

# Deterministic vs Non Deterministic values

We have various function for generating fake values:

- generate
- generateNonDeterministic
- generateNonDeterministicWithFixedSeed

By default, `generate` produces deterministic values. It's performance
is better than the others and for cases where we are going to generate
a single fake value using record type, it's a good default to
have. Example:

``` haskell
{-#LANGUAGE RecordWildCards#-}

import Faker
import Faker.Name
import Faker.Address
import Data.Text

data Person = Person {
    personName :: Text,
    personAddress :: Text
} deriving (Show, Eq)

fakePerson :: Fake Person
fakePerson = do
    personName <- name
    personAddress <- fullAddress
    pure $ Person{..}

main :: IO ()
main = do
    person <- generate fakePerson
    print person
```

And executing it, you will get:

``` haskell
Person
  { personName = "Sherryl Steuber"
  , personAddress = "Apt. 298 340 Ike Mission, Goldnertown, FL 19488-9259"
  }
```

While, it's a good default we would need non deterministic output for
certain cases:

``` haskell
> generate $ listOf 5 $ fromRange (1,100)
[39,39,39,39,39]
> generate $ listOf 5 $ fromRange (1,100)
[39,39,39,39,39]
> generateNonDeterministic $ listOf 5 $ fromRange (1,100)
[94,18,17,48,17]
> generateNonDeterministic $ listOf 5 $ fromRange (1,100)
[15,2,47,85,94]
```

Not how `generateNonDeterministic` is generating different values each
time. If you instead want to have a fixed seed, you should use
`generateNonDeterministicWithFixedSeed` instead:

``` haskell
> generateNonDeterministicWithFixedSeed $ listOf 5 $ fromRange (1,100)
[98,87,77,33,98]
> generateNonDeterministicWithFixedSeed $ listOf 5 $ fromRange (1,100)
[98,87,77,33,98]
```

# Combinators

## listOf

``` haskell
λ> import Faker.Address
λ> item <- generateNonDeterministic $ listOf 5 country
λ> item
["Ecuador","French Guiana","Faroe Islands","Canada","Armenia"]
```

## oneOf

``` haskell
λ> item <- generate $ oneof [country, fullAddress]
λ> item
"Suite 599 599 Brakus Flat, South Mason, MT 59962-6876"
```

## suchThat

``` haskell
λ> import qualified Faker.Address as AD
λ> item :: Text <- generate $ suchThat AD.country (\x -> (T.length x > 5))
λ> item
"Ecuador"
λ> item :: Text <- generate $ suchThat AD.country (\x -> (T.length x > 8))
λ> item
"French Guiana"
```

For seeing the full list of combinators, see the module documentation of
`Faker.Combinators`.


# Using the `FakeT` transformer

When generating values, you may want to perform some side-effects.

```haskell
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text
import Data.Text.IO
import Faker.ChuckNorris

logQuote :: (MonadIO m, MonadLogger m) => m ()
logQuote = do
  userName <- liftIO getLine
  quote <- generateNonDeterministic fact
  $(logInfo) $ "Chuck Norris" userName quote
```

This works fine for one-off generation - but if you try to repeatedly
generate values, you will run into performance trouble.

```haskell
import Control.Monad (replicateM)

slowFunction :: (MonadIO m, MonadLogger m) => m ()
slowFunction = replicateM 1000 logQuote
```

This is because generating a `Fake` parses the data files and builds a
cache for future use. Using the `Monad` instance on `Fake` shares that
cache between `Fake`s, making faking fast. But in the above code, a
new `Fake` is generated each time - so the cache is discarded, and
performance is much worse.

It's better to use the `FakeT` monad transformer when writing such code,
to get the benefits of sharing the cache, as well as being able to
perform side effects. `FakeT` comes with the `mtl`-style `MonadFake`
class, for easy use with your monad stack, which lets you lift `Fake`s
with `liftFake`.

```haskell
import Faker.Class

betterLogQuote :: (MonadIO m, MonadLogger m, MonadFake m) => m ()
betterLogQuote = do
  userName <- liftIO getLine
  quote <- liftFake fact
  $(logInfo) $ "Chuck Norris" userName quote
```

`slowFunction` can be rewritten to be much faster, because the `FakeT`
is shared between all the calls to `fact`.

```haskell
fastFunction :: (MonadIO m, MonadLogger m) => m ()
fastFunction = generateNonDeterministic go
  where
    go :: FakeT m ()
    go = replicateM 1000 logQuote
```

# Comparision with other libraries

There are two other libraries in the Hackage providing fake data:

- [faker](https://hackage.haskell.org/package/faker-0.0.0.2)
- [fake](https://hackage.haskell.org/package/fake-0.1.1.1)

The problem with both the above libraries is that the library covers
only a very small amount of fake data source. I wanted to have an
equivalent functionality with something like [faker](https://github.com/stympy/faker). Also, most of
the combinators in this packages has been inspired (read as taken)
from the `fake` library. Also, `fakedata` offers fairly good amount of
support of different locales. Also since we rely on an external data
source, we get free updates and high quality data source with little
effort. Also, it's easier to extend the library with [it's own data
source](https://github.com/psibi/fakedata/blob/master/Development.md#custom-fake-source-support-with-yml-file) if we want to do it that way.

# Acknowledgments

Benjamin Curtis for his [Ruby faker library](https://github.com/stympy/faker) from which the data
source is taken from.

Icons made by [Freepik](https://www.flaticon.com/authors/freepik) from [Flaticon](https://www.flaticon.com/).
