# Changelog for fakedata

## Unreleased

* Add support for regex fake value generation. Useful for postcode
  functions for different locales.

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
