{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Internal module used for configuration purposes. You don't likely
-- have to use it.
module Config
  ( SourceData(..)
  , fetchData
  , fetchDataSingle
  , mapSource
  , populateLocales
  ) where

import Control.Monad (filterM)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal.Types (CacheFileKey(..), SourceData(..))
import Language.Haskell.TH (Name)
import Paths_fakedata (getDataFileName)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((<.>), (</>), takeExtension, takeFileName)

localesDirectory :: FilePath
localesDirectory = "faker/lib/locales"

localesEnDirectory :: FilePath
localesEnDirectory = "faker/lib/locales/en"

localesCustomEnDirectory :: FilePath
localesCustomEnDirectory = "customFakeSource/en"

isLocaleFile :: FilePath -> IO Bool
isLocaleFile fname = do
  exist <- doesFileExist fname
  pure (exist && (takeExtension fname == ".yml"))

listLocaleFiles :: FilePath -> IO [FilePath]
listLocaleFiles fname = do
  files <- listDirectory fname
  filterM isLocaleFile files

populateLocales :: IO [Text]
populateLocales = do
  files <- listLocaleFiles localesDirectory
  let files' = map (pack . takeFileName) files
  pure files'

sourceFile :: SourceData -> FilePath
sourceFile Address = "address"
sourceFile Name = "name"
sourceFile Ancient = "ancient"
sourceFile Animal = "animal"
sourceFile App = "app"
sourceFile Appliance = "appliance"
sourceFile ATHF = "aqua_teen_hunger_force"
sourceFile Artist = "artist"
sourceFile BTTF = "back_to_the_future"
sourceFile Bank = "bank"
sourceFile Beer = "beer"
sourceFile BoJackHorseman = "bojack_horseman"
sourceFile Book = "book"
sourceFile BossaNova = "bossa_nova"
sourceFile BreakingBad = "breaking_bad"
sourceFile Buffy = "buffy"
sourceFile Business = "business"
sourceFile Cannabis = "cannabis"
sourceFile BigBangTheory = "big_bang_theory"
sourceFile Cat = "cat"
sourceFile ChuckNorris = "chuck_norris"
sourceFile Code = "code"
sourceFile Coffee = "coffee"
sourceFile Coin = "coin"
sourceFile Color = "color"
sourceFile Horse = "horse"
sourceFile Commerce = "commerce"
sourceFile Community = "community"
sourceFile Compass = "compass"
sourceFile Company = "company"
sourceFile Construction = "construction"
sourceFile Cosmere = "cosmere"
sourceFile CryptoCoin = "crypto_coin"
sourceFile CultureSeries = "culture_series"
sourceFile Currency = "currency"
sourceFile DcComics = "dc_comics"
sourceFile Demographic = "demographic"
sourceFile Dessert = "dessert"
sourceFile Device = "device"
sourceFile Dog = "dog"
sourceFile Dota = "dota"
sourceFile DrWho = "dr_who"
sourceFile DragonBall = "dragon_ball"
sourceFile DumbAndDumber = "dumb_and_dumber"
sourceFile Dune = "dune"
sourceFile Educator = "educator"
sourceFile ElderScrolls = "elder_scrolls"
sourceFile ElectricalComponents = "electrical_components"
sourceFile Esport = "esport"
sourceFile Fallout = "fallout"
sourceFile FamilyGuy = "family_guy"
sourceFile File = "file"
sourceFile Finance = "finance"
sourceFile Food = "food"
sourceFile Football = "football"
sourceFile FreshPrinceOfBelAir = "fresh_prince_of_bel_air"
sourceFile Friends = "friends"
sourceFile FunnyName = "funny_name"
sourceFile GameOfThrones = "game_of_thrones"
sourceFile Gender = "gender"
sourceFile GhostBusters = "ghostbusters"
sourceFile GratefulDead = "grateful_dead"
sourceFile GreekPhilosophers = "greek_philosophers"
sourceFile Hacker = "hacker"
sourceFile HalfLife = "half_life"
sourceFile HarryPotter = "harry_potter"
sourceFile Heroes = "heroes"
sourceFile HeroesOfTheStorm = "heroes_of_the_storm"
sourceFile HeyArnold = "hey_arnold"
sourceFile Hipster = "hipster"
sourceFile HitchhikersGuideToTheGalaxy = "hitchhikers_guide_to_the_galaxy"
sourceFile Hobbit = "hobbit"
sourceFile House = "house"
sourceFile HowIMetYourMother = "how_i_met_your_mother"
sourceFile IdNumber = "id_number"
sourceFile IndustrySegments = "industry_segments"
sourceFile Internet = "internet"
sourceFile Invoice = "invoice"
sourceFile Job = "job"
sourceFile Kpop = "kpop"
sourceFile LeagueOfLegends = "league_of_legends"
sourceFile Lebowski = "lebowski"
sourceFile LordOfTheRings = "lord_of_the_rings"
sourceFile Lorem = "lorem"
sourceFile LoveCraft = "lovecraft"
sourceFile Markdown = "markdown"
sourceFile Marketing = "marketing"
sourceFile Measurement = "measurement"
sourceFile MichaelScott = "michael_scott"
sourceFile Military = "military"
sourceFile Movie = "movie"
sourceFile Music = "music"
sourceFile Myst = "myst"
sourceFile Nation = "nation"
sourceFile NatoPhoneticAlphabet = "nato_phonetic_alphabet"
sourceFile NewGirl = "new_girl"
sourceFile OnePiece = "one_piece"
sourceFile OverWatch = "overwatch"
sourceFile ParksAndRec = "parks_and_rec"
sourceFile Phish = "phish"
sourceFile PhoneNumber = "phone_number"
sourceFile Pokemon = "pokemon"
sourceFile PrincessBride = "princess_bride"
sourceFile ProgrammingLanguage = "programming_language"
sourceFile Quote = "quote"
sourceFile Relationship = "relationship"
sourceFile Restaurant = "restaurant"
sourceFile RickAndMorty = "rick_and_morty"
sourceFile RockBand = "rock_band"
sourceFile Rupaul = "rupaul"
sourceFile Science = "science"
sourceFile Seinfeld = "seinfeld"
sourceFile Shakespeare = "shakespeare"
sourceFile SiliconValley = "silicon_valley"
sourceFile Simpsons = "simpsons"
sourceFile SlackEmoji = "slack_emoji"
sourceFile SonicTheHedgehog = "sonic_the_hedgehog"
sourceFile Source = "source"
sourceFile SouthPark = "south_park"
sourceFile Space = "space"
sourceFile StarTrek = "star_trek"
sourceFile StarWars = "star_wars"
sourceFile StarGate = "stargate"
sourceFile StrangerThings = "stranger_thing"
sourceFile Stripe = "stripe"
sourceFile Subscription = "subscription"
sourceFile SuperSmashBros = "super_smash_bros"
sourceFile SuperHero = "superhero"
sourceFile SwordArtOnline = "sword_art_online"
sourceFile Team = "team"
sourceFile TheExpanse = "the_expanse"
sourceFile TheItCrowd = "the_it_crowd"
sourceFile TheThickOfIt = "the_thick_of_it"
sourceFile TwinPeaks = "twin_peaks"
sourceFile UmphreysMcgee = "umphreys_mcgee"
sourceFile University = "university"
sourceFile VForVendetta = "v_for_vendetta"
sourceFile Vehicle = "vehicle"
sourceFile VentureBros = "venture_bros"
sourceFile Verbs = "verbs"
sourceFile Witcher = "witcher"
sourceFile WorldCup = "world_cup"
sourceFile WorldOfWarcraft = "world_of_warcraft"
sourceFile Yoda = "yoda"
sourceFile Zelda = "zelda"
sourceFile Basketball = "basketball"
sourceFile Opera = "opera"
sourceFile Blood = "blood"
sourceFile Chiquito = "chiquito"
sourceFile Computer = "computer"
sourceFile Control = "control"
sourceFile Departed = "departed"
sourceFile DnD = "dnd"
sourceFile PearlJam = "pearl_jam"
sourceFile Rajnikanth = "rajnikanth"
sourceFile Show = "show"
sourceFile WarhammerFantasy = "warhammer_fantasy"
sourceFile Suits = "suits"

mapSource :: Text -> Name
mapSource "pearl_jam" = 'PearlJam
mapSource "dnd" = 'DnD
mapSource "chiquito" = 'Chiquito
mapSource "departed" = 'Departed
mapSource "computer" = 'Computer
mapSource "control" = 'Control
mapSource "horse" = 'Horse
mapSource "address" = 'Address
mapSource "basketball" = 'Basketball
mapSource "name" = 'Name
mapSource "blood" = 'Blood
mapSource "ancient" = 'Ancient
mapSource "animal" = 'Animal
mapSource "app" = 'App
mapSource "appliance" = 'Appliance
mapSource "aquaTeenHungerForce" = 'ATHF
mapSource "artist" = 'Artist
mapSource "backToTheFuture" = 'BTTF
mapSource "bank" = 'Bank
mapSource "beer" = 'Beer
mapSource "bojackHourseman" = 'BoJackHorseman
mapSource "book" = 'Book
mapSource "bossaNova" = 'BossaNova
mapSource "breakingBad" = 'BreakingBad
mapSource "bigBangTheory" = 'BigBangTheory
mapSource "buffy" = 'Buffy
mapSource "business" = 'Business
mapSource "cannabis" = 'Cannabis
mapSource "cat" = 'Cat
mapSource "chuckNorris" = 'ChuckNorris
mapSource "code" = 'Code
mapSource "coffee" = 'Coffee
mapSource "coin" = 'Coin
mapSource "color" = 'Color
mapSource "commerce" = 'Commerce
mapSource "community" = 'Community
mapSource "company" = 'Company
mapSource "construction" = 'Construction
mapSource "cosmere" = 'Cosmere
mapSource "cryptoCoin" = 'CryptoCoin
mapSource "cultureSeries" = 'CultureSeries
mapSource "currency" = 'Currency
mapSource "dcComics" = 'DcComics
mapSource "demographic" = 'Demographic
mapSource "dessert" = 'Dessert
mapSource "device" = 'Device
mapSource "dog" = 'Dog
mapSource "dota" = 'Dota
mapSource "drWho" = 'DrWho
mapSource "dragonBall" = 'DragonBall
mapSource "dumbAndDumber" = 'DumbAndDumber
mapSource "dune" = 'Dune
mapSource "educator" = 'Educator
mapSource "compass" = 'Compass
mapSource "elderScrolls" = 'ElderScrolls
mapSource "electricalComponents" = 'ElectricalComponents
mapSource "esport" = 'Esport
mapSource "fallout" = 'Fallout
mapSource "familyGuy" = 'FamilyGuy
mapSource "file" = 'File
mapSource "finance" = 'Finance
mapSource "food" = 'Food
mapSource "football" = 'Football
mapSource "freshPrinceOfBelAir" = 'FreshPrinceOfBelAir
mapSource "friends" = 'Friends
mapSource "funnyName" = 'FunnyName
mapSource "gameOfThrones" = 'GameOfThrones
mapSource "gender" = 'Gender
mapSource "ghostbusters" = 'GhostBusters
mapSource "gratefulDead" = 'GratefulDead
mapSource "greekPhilosophers" = 'GreekPhilosophers
mapSource "hacker" = 'Hacker
mapSource "halfLife" = 'HalfLife
mapSource "harryPotter" = 'HarryPotter
mapSource "heroes" = 'Heroes
mapSource "heroesOfTheStorm" = 'HeroesOfTheStorm
mapSource "heyArnold" = 'HeyArnold
mapSource "hipster" = 'Hipster
mapSource "hitchhikersGuideToTheGalaxy" = 'HitchhikersGuideToTheGalaxy
mapSource "hobbit" = 'Hobbit
mapSource "house" = 'House
mapSource "howIMetYourMother" = 'HowIMetYourMother
mapSource "idNumber" = 'IdNumber
mapSource "industrySegments" = 'IndustrySegments
mapSource "internet" = 'Internet
mapSource "invoice" = 'Invoice
mapSource "job" = 'Job
mapSource "kpop" = 'Kpop
mapSource "leagueOfLegends" = 'LeagueOfLegends
mapSource "lebowski" = 'Lebowski
mapSource "lordOfTheRings" = 'LordOfTheRings
mapSource "lorem" = 'Lorem
mapSource "lovecraft" = 'LoveCraft
mapSource "markdown" = 'Markdown
mapSource "marketing" = 'Marketing
mapSource "michaelScott" = 'MichaelScott
mapSource "military" = 'Military
mapSource "movie" = 'Movie
mapSource "music" = 'Music
mapSource "myst" = 'Myst
mapSource "nation" = 'Nation
mapSource "natoPhoneticAlphabet" = 'NatoPhoneticAlphabet
mapSource "newGirl" = 'NewGirl
mapSource "onePiece" = 'OnePiece
mapSource "overwatch" = 'OverWatch
mapSource "parksAndRec" = 'ParksAndRec
mapSource "phish" = 'Phish
mapSource "phoneNumber" = 'PhoneNumber
mapSource "pokemon" = 'Pokemon
mapSource "princessBride" = 'PrincessBride
mapSource "programmingLauguage" = 'ProgrammingLanguage
mapSource "quote" = 'Quote
mapSource "relationship" = 'Relationship
mapSource "restaurant" = 'Restaurant
mapSource "rickAndMorty" = 'RickAndMorty
mapSource "rockBand" = 'RockBand
mapSource "rupaul" = 'Rupaul
mapSource "science" = 'Science
mapSource "seinfeld" = 'Seinfeld
mapSource "shakespeare" = 'Shakespeare
mapSource "siliconValley" = 'SiliconValley
mapSource "simpsons" = 'Simpsons
mapSource "slackEmoji" = 'SlackEmoji
mapSource "sonicTheHedgehog" = 'SonicTheHedgehog
mapSource "source" = 'Source
mapSource "southPark" = 'SouthPark
mapSource "space" = 'Space
mapSource "starTrek" = 'StarTrek
mapSource "starWars" = 'StarWars
mapSource "stargate" = 'StarGate
mapSource "strangerThings" = 'StrangerThings
mapSource "stripe" = 'Stripe
mapSource "subscription" = 'Subscription
mapSource "superSmashBros" = 'SuperSmashBros
mapSource "superhero" = 'SuperHero
mapSource "swordArtOnline" = 'SwordArtOnline
mapSource "team" = 'Team
mapSource "theExpanse" = 'TheExpanse
mapSource "theItCrowd" = 'TheItCrowd
mapSource "theThickOfIt" = 'TheThickOfIt
mapSource "twinPeaks" = 'TwinPeaks
mapSource "umphreysMcgee" = 'UmphreysMcgee
mapSource "university" = 'University
mapSource "vForVendetta" = 'VForVendetta
mapSource "vehicle" = 'Vehicle
mapSource "ventureBros" = 'VentureBros
mapSource "verbs" = 'Verbs
mapSource "witcher" = 'Witcher
mapSource "worldCup" = 'WorldCup
mapSource "worldOfWarcraft" = 'WorldOfWarcraft
mapSource "yoda" = 'Yoda
mapSource "zelda" = 'Zelda
mapSource "measurement" = 'Measurement
mapSource "opera" = 'Opera
mapSource "rajnikanth" = 'Rajnikanth
mapSource "show" = 'Show
mapSource "suits" = 'Suits
mapSource "warhammerFantasy" = 'WarhammerFantasy
mapSource item = error $ "mapSource: Invalid argument passed " <> (show item)

guessSourceFile :: SourceData -> Text -> FilePath
guessSourceFile sdata sysloc =
  case sysloc of
    "en" ->
      case sdata of
        Finance -> localesCustomEnDirectory </> (sourceFile sdata) <.> "yml"
        sdata' -> localesEnDirectory </> (sourceFile sdata') <.> "yml"
    oth -> localesDirectory </> (unpack oth <> ".yml")

getSourceFile :: (MonadThrow m, MonadIO m) => FilePath -> m FilePath
getSourceFile fname = do
  fname' <- liftIO $ getDataFileName fname
  exist <- liftIO $ doesFileExist fname'
  if exist
    then pure fname'
    else throwM $ InvalidLocale fname'

fetchData ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> SourceData
  -> (FakerSettings -> Value -> Parser a)
  -> m a
fetchData settings sdata parser = do
  let locale = getLocale settings
      fname = guessSourceFile sdata locale
      ckey = CacheFileKey {cfkSource = sdata, cfkLocale = locale}
  cache <- liftIO $ getCacheFile settings
  case (HM.lookup ckey cache) of
    Nothing -> do
      afile <- getSourceFile fname
      bs <- liftIO $ BS.readFile afile
      yaml <- decodeThrow bs
      let nhash = HM.insert ckey yaml cache
      liftIO $ setCacheFile nhash settings
      either (throwM . ParseError) pure (parseEither (parser settings) yaml)
    Just yaml ->
      either (throwM . ParseError) pure (parseEither (parser settings) yaml)

fetchDataSingle ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> SourceData
  -> (FakerSettings -> Value -> Parser Text)
  -> m (Vector Text)
fetchDataSingle settings sdata parser = do
  let locale = getLocale settings
      fname = guessSourceFile sdata locale
      ckey = CacheFileKey {cfkSource = sdata, cfkLocale = locale}
  cache <- liftIO $ getCacheFile settings
  case (HM.lookup ckey cache) of
    Nothing -> do
      afile <- getSourceFile fname
      bs <- liftIO $ BS.readFile afile
      yaml <- decodeThrow bs
      let nhash = HM.insert ckey yaml cache
      liftIO $ setCacheFile nhash settings
      pure <$>
        either (throwM . ParseError) pure (parseEither (parser settings) yaml)
    Just yaml ->
      pure <$>
      either (throwM . ParseError) pure (parseEither (parser settings) yaml)
