{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Faker.Internal.Types where

import Data.Hashable
import Data.Text (Text)
import GHC.Generics

data SourceData
  = Address
  | Name
  | Ancient
  | Animal
  | App
  | Appliance
  | ATHF
  | Artist
  | Basketball
  | BTTF
  | Bank
  | Beer
  | BoJackHorseman
  | Book
  | Blood
  | Chiquito
  | Computer
  | BossaNova
  | BreakingBad
  | Buffy
  | Business
  | Cannabis
  | Cat
  | ChuckNorris
  | Code
  | Coffee
  | Coin
  | Color
  | Commerce
  | Community
  | Company
  | Construction
  | Cosmere
  | Compass
  | CryptoCoin
  | CultureSeries
  | Currency
  | DcComics
  | Demographic
  | Dessert
  | Device
  | Dog
  | Dota
  | DrWho
  | DragonBall
  | DumbAndDumber
  | Dune
  | Educator
  | ElderScrolls
  | ElectricalComponents
  | Esport
  | Fallout
  | FamilyGuy
  | File
  | Finance
  | Food
  | Football
  | FreshPrinceOfBelAir
  | Friends
  | FunnyName
  | GameOfThrones
  | Gender
  | GhostBusters
  | GratefulDead
  | GreekPhilosophers
  | Hacker
  | HalfLife
  | HarryPotter
  | Heroes
  | HeroesOfTheStorm
  | HeyArnold
  | Hipster
  | HitchhikersGuideToTheGalaxy
  | Hobbit
  | House
  | HowIMetYourMother
  | IdNumber
  | IndustrySegments
  | Internet
  | Invoice
  | Job
  | Kpop
  | LeagueOfLegends
  | Lebowski
  | LordOfTheRings
  | Lorem
  | LoveCraft
  | Markdown
  | Marketing
  | Measurement
  | MichaelScott
  | Military
  | Movie
  | Music
  | Myst
  | Nation
  | NatoPhoneticAlphabet
  | NewGirl
  | OnePiece
  | Horse
  | OverWatch
  | ParksAndRec
  | Phish
  | PhoneNumber
  | Pokemon
  | PrincessBride
  | ProgrammingLanguage
  | Quote
  | Relationship
  | Restaurant
  | RickAndMorty
  | RockBand
  | Rupaul
  | Science
  | Seinfeld
  | Shakespeare
  | SiliconValley
  | Simpsons
  | SlackEmoji
  | SonicTheHedgehog
  | Source
  | SouthPark
  | Space
  | StarTrek
  | StarWars
  | StarGate
  | StrangerThings
  | Stripe
  | Subscription
  | SuperSmashBros
  | SuperHero
  | SwordArtOnline
  | Team
  | TheExpanse
  | TheItCrowd
  | TheThickOfIt
  | TwinPeaks
  | UmphreysMcgee
  | University
  | VForVendetta
  | Vehicle
  | VentureBros
  | Verbs
  | Witcher
  | WorldCup
  | WorldOfWarcraft
  | Yoda
  | Zelda
  | Opera
  | CustomSourceEnglish String
  deriving (Show, Eq, Ord, Generic, Hashable)

data CacheFieldKey =
  CacheFieldKey
    { ckSource :: !Text
    , ckField :: !Text
    , ckLocale :: !Text
    }
  deriving (Show, Eq, Ord, Generic, Hashable)

data CacheFileKey =
  CacheFileKey
    { cfkSource :: !SourceData
    , cfkLocale :: !Text
    }
  deriving (Show, Eq, Ord, Generic, Hashable)
