{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}

module Faker.Internal.Types where

import Data.Hashable
import Data.Text (Text)
import GHC.Generics
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
#endif

#if MIN_VERSION_aeson(2,0,0)
type AesonKey = K.Key
#else
type AesonKey = Text
#endif

aesonKeyToText :: AesonKey -> Text
#if MIN_VERSION_aeson(2,0,0)
aesonKeyToText key = K.toText key
#else
aesonKeyToText key = key
#endif

aesonKeyFromText :: Text -> AesonKey
#if MIN_VERSION_aeson(2,0,0)
aesonKeyFromText key = K.fromText key
#else
aesonKeyFromText key = key
#endif

data SourceData
  = Address
  | Name
  | Ancient
  | Adjective
  | Animal
  | App
  | Appliance
  | ATHF
  | Artist
  | Basketball
  | BTTF
  | Bank
  | Bird
  | Beer
  | BoJackHorseman
  | BigBangTheory
  | Book
  | Blood
  | Camera
  | ClashOfClans
  | Chiquito
  | Computer
  | Control
  | Conan
  | DnD
  | Doraemon
  | PearlJam
  | Departed
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
  | FinalSpace
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
  | HowToTrainYourDragon
  | JackHandey
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
  | Mountain
  | Music
  | Myst
  | Nation
  | Naruto
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
  | Room
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
  | SuperMario
  | SuperHero
  | SwordArtOnline
  | Tea
  | Team
  | TheExpanse
  | TheItCrowd
  | TheThickOfIt
  | TwinPeaks
  | Tolkien
  | Touhou
  | UmphreysMcgee
  | University
  | VForVendetta
  | Vehicle
  | VentureBros
  | Verbs
  | Volleyball
  | Witcher
  | WorldCup
  | WorldOfWarcraft
  | Yoda
  | Zelda
  | Opera
  | Rajnikanth
  | Show
  | Suits
  | WarhammerFantasy
  | Barcode
  | DrivingLicense
  | Drone
  | Futurama
  | Minecraft
  | Prince
  | Rush
  | StreetFighter
  | StudioGhibli
  | CustomSourceEnglish String
  deriving (Show, Eq, Ord, Generic, Hashable)

data CacheFieldKey =
  CacheFieldKey
    { ckSource :: !Text
    , ckField :: !AesonKey
    , ckLocale :: !Text
    }
  deriving (Show, Eq, Ord, Generic, Hashable)

data CacheFileKey =
  CacheFileKey
    { cfkSource :: !SourceData
    , cfkLocale :: !Text
    }
  deriving (Show, Eq, Ord, Generic, Hashable)
