# fakedata

This library is a port of Ruby's
[faker](https://github.com/stympy/faker). Note that it directly uses
the source data from that library, so the quality of fake data is
quite high!

## Tutorial

### Generating address

``` shellsession
~/g/fakedata (master) $ stack ghci
λ> import Faker.Address
λ> address <- generate fullAddress
λ> address
"Suite 153 153 Langosh Way, East Antony, MI 15342-5123"
```

### Generating name

``` shellsession
λ> fullName <- generate name
λ> fullName
"Antony Langosh"
```

### Generate quotes from the movie [Back to the Future](https://en.wikipedia.org/wiki/Back_to_the_Future)

```
λ> import Faker.BackToTheFuture
λ> import Faker.Combinators
λ> qs <- generate $ listOf 5 quote
λ> qs
[ "Yes. Yes. I'm George. George McFly. I'm your density. I mean, your destiny."
, "Hello? Hello? Anybody home? Huh? Think, McFly. Think! I gotta have time to get them retyped. Do you realize what would happen if I hand in my reports in your handwriting? I'll get fired. You wouldn't want that to happen, would ya? Would ya?"
, "Lorraine. My density has brought me to you."
, "See you in about 30 years."
, "You really think I ought to swear?"
]

```

### Combining Fake datas

```haskell
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

```
$ stack name.hs
Person {personName = "Antony Langosh", personAddress = "Suite 599 599 Brakus Flat, South Mason, MT 59962-6876"}
```

You would have noticed in the above output that the name and address
are the same as generated before in the GHCi REPL. That's because, by
default all the generated data are deterministic. If you want a
different set of ouput each time, you would have to modify the random
generator output:

```
main :: IO ()
main = do
    gen <- newStdGen
    let settings = setRandomGen gen defaultFakerSettings
    person <- generateWithSettings settings fakePerson
    print person
```

And on executing the program, you will get a different output:

``` shellsession
Person {personName = "Ned Effertz Sr.", personAddress = "Suite 158 1580 Schulist Mall, Schulistburgh, NY 15804-3392"}
```

The above program can be even minimized like this:

``` haskell
main :: IO ()
main = do
    let settings = setNonDeterministic defaultFakerSettings
    person <- generateWithSettings settings fakePerson
    print person
```

### Combinators

#### listOf

``` haskell
λ> import Faker.Address
λ> item <- generate $ listOf 5 country
λ> item
["Ecuador","French Guiana","Faroe Islands","Canada","Armenia"]
```

#### oneOf

``` haskell
λ> item <- generate $ oneof [country, fullAddress]
λ> item
"Suite 599 599 Brakus Flat, South Mason, MT 59962-6876"
```

For seeing the full list of combinators, see the module documentation
of `Faker.Combinators`.

# Comparision with other libraries

There are two other libraries in the Hackage providing fake data:

* [faker](https://hackage.haskell.org/package/faker-0.0.0.2)
* [fake](https://hackage.haskell.org/package/fake-0.1.1.1)

The problem (for me) with both the above libraries is that the library
covers only a very small amount of fake data source. I wanted to have
an equivalent functionality with something like
[faker](https://github.com/stympy/faker). Also, most of the
combinators in this packages has been inspired (read as taken) from
the `fake` library.

# Todo

* company - buzzword, bs. Implementation of type in Faker.Company.hs
* compass.yml -- finish this first. Need to provider the Faker module and write test for it. You may also have to modify the gen script slightly.
* finance
