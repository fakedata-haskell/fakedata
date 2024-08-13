# Changelog for fakedata

## 1.0.4

* Only generate fake email domains with alphanumeric characters and hyphens. [#53](https://github.com/fakedata-haskell/fakedata/pull/53)

Updated according to jezen's pull-request with his permission. 

## 1.0.3

* [Make the `Fake` type synonym partially applied](https://github.com/fakedata-haskell/fakedata/pull/45)
* Implement deterministic output with fixed seed.

This is the behavior of `generateNonDeterministic` vs `generate`
function:

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

As you can see with `generate` we get deterministic value output and
they are all same in this case. With `generateNonDeterministic`
function we use a different seed each time which results in generating
different values.

There are times when we instead want to generate non deterministic
values but with a fixed seed. And for such use case, we have
implemented `generateNonDeterministicWithFixedSeed` function:

``` haskell
> generateNonDeterministicWithFixedSeed $ listOf 5 $ fromRange (1,100)
[98,87,77,33,98]
> generateNonDeterministicWithFixedSeed $ listOf 5 $ fromRange (1,100)
[98,87,77,33,98]
```

## 1.0.2

* Make it compatible with aeson 1 and 2.

## 1.0.1

* Improve README with use cases
* [Cleanup dependencies](https://github.com/fakedata-haskell/fakedata/pull/38)

## 1.0

### Breaking changes

* [Make `Fake` as monad transformer](https://github.com/fakedata-haskell/fakedata/pull/32). This makes writing code with good performance easily.
* Rename `celebrities` to `actors` in Faker.TvShow.Buffy module.
* Rename `celebrities` to `actors` in Faker.TvShow.FreshPrinceOfBelAir module.
* Removed module `Faker.Movie.Hobbit`. Use `Faker.Fantasy.Tolkien`
  instead.
* Removed module `Faker.Movie.LordOfTheRings`. Use
  `Faker.Fantasy.Tolkien` instead.
* `Faker.Minecraft` moved to `Faker.Game.Minecraft`
* Faker.Game.WorldOfWarcraft: `hero` function renamed to `heros`

### New modules introduced

* Faker.Adjective
* Faker.Creature.Bird
* Faker.Camera
* Faker.Game.ClashOfClans
* Faker.JapaneseMedia.Conan
* Faker.JapaneseMedia.Doraemon
* Faker.TvShow.FinalSpace
* Faker.Movie.HowToTrainYourDragon
* Faker.Quote.JackHandey
* Faker.Mountain
* Faker.JapaneseMedia.Naruto
* Faker.Movie.Room
* Faker.Game.SuperMario
* Faker.Tea
* Faker.Fantasy.Tolkien
* Faker.Game.Touhou
* Faker.Sport.Volleyball

### Module updates

* Faker.JapaneseMedia.DragonBall: `races`, `planets` added.
* Faker.DrivingLicense: `usaNorthDakota` added.
* Faker.Book.Dune: `cities` added
* Faker.Educator: `primary` and `primarySchool` added.
* Faker.Game.ElderScrolls: `weapon` and `jewelry` added
* Faker.Finance: `tickerNasdaq` and `tickerNyse` added
* Faker.Game.Heroes: `artifacts` added.
* Faker.Military: `coast_guard_rank` and `space_force_rank` added.
* Faker.Game.Mincecraft: Following functions are added
  - achievement
  - biome
  - enchantment
  - gameMode
  - statusEffect
* Faker.Music: `mamboNo5` and hiphop functions added.
* Various new functions added to Faker.Opera
* Faker.Quote: `fortuneCookie` added.
* Faker.Music.RockBand: `song` added.
* Faker.Science: `elementState` and `elementSubCategory` added.
* Various new functions added to Faker.Source.
* Faker.Game.Witcher: `signs`, `potions` and `books` added.
* Faker.Game.WorldOfWarcraft: `classNames` and `races` added.

### Locale Improvements

* es-AR localed added
* id.yml: stateAbbr added
* pt-BR: countryCode added, ingredients and licensePlate updated.
* ru: Faker.Yoda.quotes added
* uk: Faker.Address.fullAddress added
* ja: Faker.Book, Faker.Commerce and Faker.Subscription module works now
* fr: Faker.Creature.Animal, Faker.Gender works now

### Data Update

The following data sources which the libraries uses has been updated:

* de.yml
* en/animal.yml
* en/book.yml
* en/company.yml
* en/demographic.yml
* en/device.yml
* en/dota.yml
* en/fallout.yml
* en/football.yml
* en/half_life.yml
* en/heroes_of_the_storm.yml
* en/horse.yml
* en/kpop.yml
* en/league_of_legends.yml
* en/lebowski.yml
* en/lovecraft.yml
* en/myst.yml
* en/overwatch.yml
* en/pokemon.yml
* en/shakespeare.yml
* en/space.yml
* en/street_fighter.yml
* en/studio_ghibli.yml
* en/super_smash_bros.yml
* en-us.yml
* en/zelda.yml

## 0.8.0

### Breaking changes

* Behavior of Monad instances changed. Monad instance by default will
  not change the underlying StdGen by default. It will change only
  when you do `setNonDeterministic` for the settings. If you use
  functions like `listOf` with `generate` function then all the values
  would be the same:

``` haskell
λ> import Faker.Coffee
λ> qs <- generate $ listOf 5 blendName
λ> qs
["The Treat","The Treat","The Treat","The Treat","The Treat"]
```

  Now instead you have to use `generateNonDeterministic` function:

``` haskell
λ> qs <- generateNonDeterministic $ listOf 5 blendName
λ> qs
["The Treat","Evening Cowboy","Veranda Forrester","Blue Choice","Veranda Cake"]
```

* Faker.Dnd: Remove species and background function. Other new
  function introduced. See Module update section.
* Faker.Game.HeroesOfTheStorm: Rename classes function to classNames

### New modules introduced

* Faker.TvShow.BigBangTheory
* Faker.Barcode
* Faker.DrivingLicense
* Faker.Drone
* Faker.TvShow.Futurama
* Faker.Game.Minecraft
* Faker.Music.Prince
* Faker.Music.Rush
* Faker.Game.StreetFighter
* Faker.JapaneseMedia.StudioGhibli

### Locale Improvements

* en-GB: Faker.Address.postcode
* nl: Faker.Address.postcode
* ru: Faker.Company.name
* de-AT: PhoneNumber.countryCode
* de-CH: PhoneNumber.countryCode
* de: PhoneNumber.countryCode
* en-au-ocker: PhoneNumber.countryCode
* en-IND: PhoneNumber.countryCode
* en-MS: PhoneNumber.countryCode
* en-NEP: PhoneNumber.countryCode
* en-NZ: PhoneNumber.countryCode
* en-SG: PhoneNumber.countryCode
* es: Vehicle.licensePlate
* fr-CA: PhoneNumber.cellPhoneFormat, PhoneNumber.countryCode
* fr: PhoneNumber.countryCode, Compass.direction
* id: PhoneNumber.countryCode
* it: PhoneNumber.countryCode
* ko: Color.name, Space.planet, Space.galaxy, Gender.binaryTypes
* nb-NO: PhoneNumber.countryCode
* pt: PhoneNumber.countryCode
* ru: PhoneNumber.countryCode
* sk: PhoneNumber.countryCode

### Module updates

* Faker.TvShow.AquaTeenHungerForce: quote
* Faker.DND: cities, languages, meleeWeapons, monsters, races, rangedWeapons
* Faker.TvShow.Simpsons: episodeTitles
* Faker.Movie.StarWars: quotesKyloRen

### Data Update

The following data sources which the libraries uses has been updated:

* en/animal.yml
* en/aqua_teen_hunger_force.yml
* en-AU.yml
* en/bank.yml
* en-GB.yml
* en-IND.yml
* en/star_trek.yml
* en-US.yml
* fi-FI.yml
* fr-CA.yml
* fr-CH.yml
* fr.yml
* id.yml
* it.yml

## 0.7.1

* Add combinator for `fakeBoundedEnum` In Faker.Combinators
* Improved Haddock documentation for Faker.Combinators

## 0.7.0

* Add support for regex fake value generation. Useful for postcode
  functions for different locales.
* Improve documentation of Faker.Combinators.
* `de-CH` locale update: `lastName` function works now.
* New function in Faker.Music.Phish module: albums, musicians
* New function in Faker.Address: cityWithState
* New function in Faker.Movie: title

### Data Update

The following data sources which the libraries uses has been updated:

* heroes_of_the_storm.yml
* house.yml
* name.yml
* one_piece.yml

### New modules introduced

* Faker.Blood
* Faker.Chiquito
* Faker.Computer
* Faker.Game.Control
* Faker.Movie.Departed
* Faker.Dnd
* Faker.Music.PearlJam
* Faker.Rajnikanth
* Faker.Show
* Faker.TvShow.Suits
* Faker.WarhammerFantasy

### Locale Improvements

* en-AU:
  - Locale has updated which leads to working of newer functions:
    Faker.Name.prefix, Faker.University.name, Faker.Bank.name
* en-CA: Faker.Address.postcode works now.
* en-GB: formats field has been updated.
* fr-CA: Faker.Address.postcode works now.
* ko: Supports Faker.Commerce moudle now.

### Breaking changes

* Faker.Music.Phish module:
  - Rename `song` function to `songs`

## 0.6.1

* Add `Semigroup` and `Monoid` instances to `Fake`
* Doc fix: Remove broken links

## 0.6.0

* Fix API for "ar" locale. Add test coverage.
* Bug fixed in the following locales: `ca`, `bg`, `da-DK`,
  `en-NEP`, `en-ZA`, `fr-CA`, `fr-CH`, `fr`, `hy`, `id`, `ja`, `pt`,
  `uk`, `zh-CN`, `zh-TW`
* New function to Address module: `mailBox`
* Update in following data sources:
  - en/color.yml
  - en-NZ.yml
  - en/overwatch.yml
  - en/phone_number.yml
  - en/shakespeare.yml
  - fa.yml
  - pt-BR.yml
* New function in Educator module: `degree`, `courseName`
* New function in Gender module: `shortBinaryTypes`

### Breaking changes

* Educator module:
  - Rename `tertiaryDegreeSubject` to `subject`
  - Rename `name` function to `schoolName`
  - Rename `tertiaryType` function to `tertiaryUniversityType`.

## 0.5.0

* Move remaining internal modules into other-module: Config, Faker.TH

## 0.4.0

* Move Provider modules into other-modules. This makes the haddock
  much more readable.
* Update to various `en` data sources: color.yml, super_smash_bros.
* Fix name of FakerTvShow.DrWho module: villains (from villians).
* New locale addition: en-TH, th
* Modification of other locales: fr-CA, ja

## 0.3.1

* Make it compatbile with ghc-8.8.1

## 0.3.0

* Update fake data source
* Improve performance and add test coverage
* Fix various bugs
* Add `brands` function to the module Faker.Cannabis

## 0.2.2

* Add meepoQuote function to the module Faker.Game.Dota
* Add Faker.Music.Opera module
* Update fake data source

## 0.2.1

* Fix bug in Book module

## 0.2.0

* Add sicCode function in Company module
* New Construction module
* New Basketball module
* New Horse module
* New Finance module
* Move Football under Sport module
* Bug fixes in some file formats
* Fix bugs in the Creature module
* Add Development.md for new contributors

## 0.1.0.0

Initial version released
