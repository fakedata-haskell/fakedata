{-# LANGUAGE ScopedTypeVariables #-}

module TextSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Faker hiding (defaultFakerSettings)
import qualified Faker.Address as FA
import qualified Faker.Ancient as AN
import qualified Faker.App as AP
import qualified Faker.Appliance as AP
import qualified Faker.Artist as AR
import qualified Faker.Bank as BA
import qualified Faker.Beer as BE
import qualified Faker.Book as BO
import qualified Faker.Book.CultureSeries as CE
import qualified Faker.Book.Dune as DU
import qualified Faker.Book.Lovecraft as LO
import qualified Faker.BossaNova as BO
import qualified Faker.Business as BU
import qualified Faker.Cannabis as CA
import qualified Faker.ChuckNorris as CH
import qualified Faker.Code as CO
import qualified Faker.Coffee as CO
import qualified Faker.Coin as CO
import qualified Faker.Color as CO
import qualified Faker.Commerce as CO
import qualified Faker.Company as CM
import qualified Faker.Compass as CO
import qualified Faker.Construction as CO
import qualified Faker.Cosmere as CO
import qualified Faker.Creature.Animal as AN
import qualified Faker.Creature.Cat as CA
import qualified Faker.Creature.Dog as DO
import qualified Faker.Creature.Horse as HO
import qualified Faker.CryptoCoin as CO
import qualified Faker.Currency as CU
import qualified Faker.DcComics as DC
import qualified Faker.Demographic as DE
import qualified Faker.Dessert as DE
import qualified Faker.Educator as ED
import qualified Faker.ElectricalComponents as EC
import qualified Faker.Esport as ES
import qualified Faker.File as FI
import qualified Faker.Finance as FI
import qualified Faker.Food as FO
import qualified Faker.Football as FO
import qualified Faker.FunnyName as FU
import qualified Faker.Game.Dota as DO
import qualified Faker.Game.ElderScrolls as EL
import qualified Faker.Game.Fallout as FA
import qualified Faker.Game.HalfLife as HA
import qualified Faker.Game.Heroes as HE
import qualified Faker.Game.HeroesOfTheStorm as HS
import qualified Faker.Game.Myst as MY
import qualified Faker.Game.Overwatch as OV
import qualified Faker.Game.Pokemon as PO
import qualified Faker.Game.SonicTheHedgehog as SO
import qualified Faker.Game.SuperSmashBros as SU
import qualified Faker.Game.Witcher as WI
import qualified Faker.Game.Witcher as WI
import qualified Faker.Game.WorldOfWarcraft as WO
import qualified Faker.Game.Zelda as ZE
import qualified Faker.Gender as GE
import qualified Faker.GreekPhilosophers as GR
import qualified Faker.Hacker as HA
import qualified Faker.Hipster as HI
import qualified Faker.House as HO
import qualified Faker.IndustrySegments as IN
import qualified Faker.Internet as IN
import qualified Faker.JapaneseMedia.DragonBall as DR
import qualified Faker.JapaneseMedia.OnePiece as ON
import qualified Faker.JapaneseMedia.SwordArtOnline as SW
import qualified Faker.Job as JO
import qualified Faker.Kpop as KP
import qualified Faker.LeagueOfLegends as LE
import qualified Faker.Lorem as LM
import qualified Faker.Markdown as MA
import qualified Faker.Marketing as MA
import qualified Faker.Measurement as ME
import qualified Faker.Military as MI
import qualified Faker.Movie as MO
import qualified Faker.Movie.BackToTheFuture as BA
import qualified Faker.Movie.Ghostbusters as GH
import qualified Faker.Movie.GratefulDead as GR
import qualified Faker.Movie.HarryPotter as HA
import qualified Faker.Movie.HitchhikersGuideToTheGalaxy as HI
import qualified Faker.Movie.Hobbit as HO
import qualified Faker.Movie.Lebowski as LE
import qualified Faker.Movie.LordOfTheRings as LO
import qualified Faker.Movie.PrincessBride as PR
import qualified Faker.Movie.StarWars as ST
import qualified Faker.Movie.VForVendetta as VF
import qualified Faker.Music as MU
import qualified Faker.Music.Opera as OP
import qualified Faker.Music.Phish as PH
import qualified Faker.Music.RockBand as RO
import qualified Faker.Music.UmphreysMcgee as UM
import qualified Faker.Name as NA
import qualified Faker.Nation as NA
import qualified Faker.NatoPhoneticAlphabet as NA
import qualified Faker.PhoneNumber as PH
import qualified Faker.ProgrammingLanguage as PR

import Test.Hspec
import TestImport

isText :: Text -> Bool
isText x = T.length x >= 1

fakerSettings :: FakerSettings
fakerSettings = defaultFakerSettings

verifyFakes :: [Fake Text] -> IO [Bool]
verifyFakes funs = do
  let fs :: [IO Text] = map (generateWithSettings fakerSettings) funs
      gs :: [IO Bool] = map (\f -> isText <$> f) fs
  sequence gs

spec :: Spec
spec = do
  describe "TextSpec" $ do
    it "Address" $ do
      let functions :: [Fake Text] =
            [ FA.country
            , FA.cityPrefix
            , FA.citySuffix
            , FA.countryCode
            , FA.countryCodeLong
            , FA.buildingNumber
            , FA.communityPrefix
            , FA.communitySuffix
            , FA.community
            , FA.streetSuffix
            , FA.secondaryAddress
            , FA.postcode
            , FA.state
            , FA.stateAbbr
            , FA.timeZone
            , FA.city
            , FA.streetName
            , FA.streetAddress
            , FA.fullAddress
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Ancient" $ do
      let functions :: [Fake Text] = [AN.god, AN.primordial, AN.hero, AN.titan]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "App" $ do
      let functions :: [Fake Text] = [AP.name, AP.version]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Appliance" $ do
      let functions :: [Fake Text] = [AP.brand, AP.equipment]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Artist" $ do
      let functions :: [Fake Text] = [AR.names]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Bank" $ do
      let functions :: [Fake Text] = [BA.name, BA.swiftBic]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Beer" $ do
      let functions :: [Fake Text] =
            [BE.name, BE.brand, BE.hop, BE.yeast, BE.malt, BE.style]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.CultureSeries" $ do
      let functions :: [Fake Text] =
            [ CE.books
            , CE.cultureShips
            , CE.cultureShipClasses
            , CE.cultureShipClassAbvs
            , CE.civs
            , CE.planets
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.Dune" $ do
      let functions :: [Fake Text] =
            [ DU.characters
            , DU.titles
            , DU.planets
            , DU.quotesGuildNavigator
            , DU.quotesEmperor
            , DU.quotesPaul
            , DU.quotesThufir
            , DU.quotesJessica
            , DU.quotesIrulan
            , DU.quotesMohiam
            , DU.quotesGurney
            , DU.quotesLeto
            , DU.quotesStilgar
            , DU.quotesLietKynes
            , DU.quotesPardotKynes
            , DU.quotesBaronHarkonnen
            , DU.quotesPiter
            , DU.quotesAlia
            , DU.quotesMapes
            , DU.quotesDuncan
            , DU.quotesYueh
            , DU.sayingsBeneGesserit
            , DU.sayingsFremen
            , DU.sayingsMentat
            , DU.sayingsMuaddib
            , DU.sayingsOrangeCatholicBible
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book.LoveCraft" $ do
      let functions :: [Fake Text] =
            [LO.fhtagn, LO.deity, LO.location, LO.tome, LO.words]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Book" $ do
      let functions :: [Fake Text] =
            [BO.author, BO.title, BO.publisher, BO.genre]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "BossaNova" $ do
      let functions :: [Fake Text] = [BO.artists, BO.songs]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Business" $ do
      let functions :: [Fake Text] = [BU.creditCardNumbers, BU.creditCardTypes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cannabis" $ do
      let functions :: [Fake Text] =
            [ CA.strains
            , CA.cannabinoidAbbreviations
            , CA.cannabinoids
            , CA.terpenes
            , CA.medicalUses
            , CA.healthBenefits
            , CA.categories
            , CA.types
            , CA.buzzwords
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "ChuckNorris" $ do
      let functions :: [Fake Text] = [CH.fact]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Code" $ do
      let functions :: [Fake Text] = [CO.asin]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Coffee" $ do
      let functions :: [Fake Text] =
            [ CO.country
            , CO.variety
            , CO.intensifier
            , CO.body
            , CO.descriptor
            , CO.name1
            , CO.name2
            , CO.notes
            , CO.blendName
            , CO.regionsColombia
            , CO.regionsBrazil
            , CO.regionsSumatra
            , CO.regionsEthiopia
            , CO.regionsHonduras
            , CO.regionsKenya
            , CO.regionsUganda
            , CO.regionsMexico
            , CO.regionsGuatemala
            , CO.regionsNicaragua
            , CO.regionsCostaRica
            , CO.regionsTanzania
            , CO.regionsElSalvador
            , CO.regionsRwanda
            , CO.regionsBurundi
            , CO.regionsPanama
            , CO.regionsYemen
            , CO.regionsIndia
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Coin" $ do
      let functions :: [Fake Text] = [CO.flip]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Color" $ do
      let functions :: [Fake Text] = [CO.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Commerce" $ do
      let functions :: [Fake Text] =
            [ CO.department
            , CO.productNameAdjective
            , CO.productNameMaterial
            , CO.productNameProduct
            , CO.promotionCodeAdjective
            , CO.promotionCodeNoun
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Company" $ do
      let functions :: [Fake Text] =
            [ CM.suffix
            , CM.buzzword
            , CM.bs
            , CM.name
            , CM.industry
            , CM.profession
            , CM.type'
            , CM.sicCode
              -- Fix buzzword and bs TODO
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Company" $ do
      let functions :: [Fake Text] =
            [ CO.direction
            , CO.abbreviation
            , CO.azimuth
            , CO.cardinalWord
            , CO.cardinalAbbreviation
            , CO.cardinalAzimuth
            , CO.ordinalWord
            , CO.ordinalAbbreviation
            , CO.ordinalAzimuth
            , CO.halfWindWord
            , CO.halfWindAbbreviation
            , CO.halfWindAzimuth
            , CO.quarterWindWord
            , CO.quarterWindAbbreviation
            , CO.quarterWindAzimuth
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Construction" $ do
      let functions :: [Fake Text] =
            [ CO.materials
            , CO.subcontractCategories
            , CO.heavyEquipment
            , CO.roles
            , CO.trades
            , CO.standardCostCodes
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cosmere" $ do
      let functions :: [Fake Text] =
            [ CO.aons
            , CO.shardWorlds
            , CO.shards
            , CO.surges
            , CO.knightsRadiant
            , CO.metals
            , CO.allomancers
            , CO.feruchemists
            , CO.heralds
            , CO.sprens
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Animal" $ do
      let functions :: [Fake Text] = [AN.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Cat" $ do
      let functions :: [Fake Text] = [CA.name, CA.breed, CA.registry]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Dog" $ do
      let functions :: [Fake Text] =
            [ DO.name
            , DO.breed
            , DO.sound
            , DO.memePhrase
            , DO.age
            , DO.coatLength
            , DO.size
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Creature.Horuse" $ do
      let functions :: [Fake Text] = [HO.name, HO.breed]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Cryptocoin" $ do
      let functions :: [Fake Text] = [CO.coin]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Currency" $ do
      let functions :: [Fake Text] = [CU.name, CU.code, CU.symbol]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "DcComics" $ do
      let functions :: [Fake Text] =
            [DC.hero, DC.heroine, DC.villain, DC.name, DC.title]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Demographic" $ do
      let functions :: [Fake Text] =
            [ DE.race
            , DE.sex
            , DE.demonym
            , DE.educationalAttainment
            , DE.maritalStatus
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Dessert" $ do
      let functions :: [Fake Text] = [DE.variety, DE.topping, DE.flavor]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Educator" $ do
      let functions :: [Fake Text] =
            [ ED.name
            , ED.secondary
            , ED.tertiaryType
            , ED.tertiaryDegreeSubject
            , ED.tertiaryDegreeType
            , ED.tertiaryCourseNumber
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "ElectricalComponents" $ do
      let functions :: [Fake Text] =
            [EC.active, EC.passive, EC.electromechanical]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Esport" $ do
      let functions :: [Fake Text] =
            [ES.players, ES.teams, ES.events, ES.leagues, ES.games]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "File" $ do
      let functions :: [Fake Text] = [FI.extension, FI.mimeType]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Finance" $ do
      let functions :: [Fake Text] =
            [ FI.visa
            , FI.mastercard
            , FI.discover
            , FI.dinersClub
            , FI.jcb
            , FI.switch
            , FI.solo
            , FI.dankort
            , FI.maestro
            , FI.forbrugsforeningen
            , FI.laser
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Food" $ do
      let functions :: [Fake Text] =
            [ FO.dish
            , FO.descriptions
            , FO.ingredients
            , FO.fruits
            , FO.vegetables
            , FO.spices
            , FO.measurements
            , FO.measurementSizes
            , FO.metricMeasurements
            , FO.sushi
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Football" $ do
      let functions :: [Fake Text] =
            [FO.teams, FO.players, FO.coaches, FO.competitions, FO.positions]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "FunnyName" $ do
      let functions :: [Fake Text] = [FU.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Dota" $ do
      let functions :: [Fake Text] =
            [ DO.hero
            , DO.item
            , DO.team
            , DO.player
            , DO.abaddonQuote
            , DO.alchemistQuote
            , DO.axeQuote
            , DO.beastmasterQuote
            , DO.brewmasterQuote
            , DO.bristlebackQuote
            , DO.centaurQuote
            , DO.chaosKnightQuote
            , DO.clockwerkQuote
            , DO.doomQuote
            , DO.dragonKnightQuote
            , DO.earthSpiritQuote
            , DO.earthshakerQuote
            , DO.elderTitanQuote
            , DO.huskarQuote
            , DO.ioQuote
            , DO.kunkkaQuote
            , DO.legionCommanderQuote
            , DO.lifestealerQuote
            , DO.lycanQuote
            , DO.magnusQuote
            , DO.nightStalkerQuote
            , DO.omniknightQuote
            , DO.phoenixQuote
            , DO.pudgeQuote
            , DO.sandKingQuote
            , DO.slardarQuote
            , DO.spiritBreakerQuote
            , DO.svenQuote
            , DO.tidehunterQuote
            , DO.timbersawQuote
            , DO.tinyQuote
            , DO.tuskQuote
            , DO.underlordQuote
            , DO.undyingQuote
            , DO.wraithKingQuote
            , DO.meepoQuote
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.ElderScrolls" $ do
      let functions :: [Fake Text] =
            [ EL.race
            , EL.creature
            , EL.region
            , EL.dragon
            , EL.city
            , EL.firstName
            , EL.lastName
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Fallout" $ do
      let functions :: [Fake Text] =
            [FA.characters, FA.factions, FA.locations, FA.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.HalfLife" $ do
      let functions :: [Fake Text] = [HA.character, HA.enemy, HA.location]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Heroes" $ do
      let functions :: [Fake Text] = [HE.names, HE.specialties, HE.klasses]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.HeroesOfTheStorm" $ do
      let functions :: [Fake Text] =
            [HS.battlegrounds, HS.classes, HS.heroes, HS.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Myst" $ do
      let functions :: [Fake Text] =
            [MY.games, MY.creatures, MY.characters, MY.ages, MY.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Overwatch" $ do
      let functions :: [Fake Text] = [OV.heroes, OV.locations, OV.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Pokemon" $ do
      let functions :: [Fake Text] = [PO.names, PO.locations, PO.moves]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.SonicTheHedgehog" $ do
      let functions :: [Fake Text] = [SO.zone, SO.character, SO.game]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.SonicTheHedgehog" $ do
      let functions :: [Fake Text] = [SO.zone, SO.character, SO.game]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.SuperSmashBros" $ do
      let functions :: [Fake Text] = [SU.fighter, SU.stage]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Witcher" $ do
      let functions :: [Fake Text] =
            [ WI.characters
            , WI.witchers
            , WI.schools
            , WI.locations
            , WI.quotes
            , WI.monsters
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.WorldOfwarcraft" $ do
      let functions :: [Fake Text] = [WO.hero, WO.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Game.Zelda" $ do
      let functions :: [Fake Text] =
            [ZE.games, ZE.characters, ZE.locations, ZE.items]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Gender" $ do
      let functions :: [Fake Text] = [GE.types, GE.binaryTypes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "GreekPhilosophers" $ do
      let functions :: [Fake Text] = [GR.names, GR.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Hacker" $ do
      let functions :: [Fake Text] =
            [HA.abbreviation, HA.adjective, HA.noun, HA.verb, HA.ingverb]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Hipster" $ do
      let functions :: [Fake Text] = [HI.words]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "House" $ do
      let functions :: [Fake Text] = [HO.furniture, HO.rooms]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Industry" $ do
      let functions :: [Fake Text] =
            [IN.industry, IN.superSector, IN.sector, IN.subSector]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Internet" $ do
      let functions :: [Fake Text] =
            [ IN.freeEmail
            , IN.domainSuffix
            , IN.userAgentAol
            , IN.userAgentChrome
            , IN.userAgentFirefox
            , IN.userAgentInternetExplorer
            , IN.userAgentNetscape
            , IN.userAgentOpera
            , IN.userAgentSafari
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "JapaneseMedia.DragonBall" $ do
      let functions :: [Fake Text] = [DR.characters]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "JapaneseMedia.OnePiece" $ do
      let functions :: [Fake Text] =
            [ ON.characters
            , ON.seas
            , ON.islands
            , ON.locations
            , ON.quotes
            , ON.akumasNoMi
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "JapaneseMedia.SwordArtOnline" $ do
      let functions :: [Fake Text] =
            [SW.realName, SW.gameName, SW.location, SW.item]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Job" $ do
      let functions :: [Fake Text] =
            [ JO.field -- TODO: Make it cached
            , JO.seniority
            , JO.position
            , JO.keySkills
            , JO.employmentType
            , JO.educationLevel
            , JO.title
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Kpop" $ do
      let functions :: [Fake Text] =
            [ KP.iGroups
            , KP.iiGroups
            , KP.iiiGroups
            , KP.girlGroups
            , KP.boyBands
            , KP.solo
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "LeagueOfLegends" $ do
      let functions :: [Fake Text] =
            [ LE.champion
            , LE.location
            , LE.quote
            , LE.summonerSpell
            , LE.masteries
            , LE.rank
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Lorem" $ do
      let functions :: [Fake Text] = [LM.words, LM.supplemental]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Markdown" $ do
      let functions :: [Fake Text] = [MA.headers, MA.emphasis]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Marketing" $ do
      let functions :: [Fake Text] = [MA.buzzwords]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Measurement" $ do
      let functions :: [Fake Text] =
            [ ME.height
            , ME.length
            , ME.volume
            , ME.weight
            , ME.metricHeight
            , ME.metricLength
            , ME.metricVolume
            , ME.metricWeight
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Military" $ do
      let functions :: [Fake Text] =
            [ MI.armyRank
            , MI.marinesRank
            , MI.navyRank
            , MI.airForceRank
            , MI.dodPaygrade
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.BackToTheFuture" $ do
      let functions :: [Fake Text] = [BA.dates, BA.characters, BA.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.Ghostbusters" $ do
      let functions :: [Fake Text] = [GH.actors, GH.characters, GH.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.GratefulDead" $ do
      let functions :: [Fake Text] = [GR.players, GR.songs]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.HarryPotter" $ do
      let functions :: [Fake Text] =
            [ HA.characters
            , HA.locations
            , HA.quotes
            , HA.books
            , HA.houses
            , HA.spells
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.HitchhikersGuideToTheGalaxy" $ do
      let functions :: [Fake Text] =
            [ HI.characters
            , HI.locations
            , HI.marvinQuote
            , HI.planets
            , HI.quotes
            , HI.species
            , HI.starships
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.Hobbit" $ do
      let functions :: [Fake Text] =
            [HO.character, HO.thorinsCompany, HO.quote, HO.location]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.Lebowski" $ do
      let functions :: [Fake Text] = [LE.actors, LE.characters, LE.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.LordOfTheRings" $ do
      let functions :: [Fake Text] = [LO.characters, LO.locations, LO.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.PrincessBride" $ do
      let functions :: [Fake Text] = [PR.characters, PR.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.StarWars" $ do
      let functions :: [Fake Text] =
            [ ST.characters
            , ST.callSquadrons
            , ST.droids
            , ST.planets
            , ST.species
            , ST.vehicles
            , ST.wookieeWords
            , ST.callNumbers
            , ST.callSign
            , ST.quotesAdmiralAckbar
            , ST.quotesAhsokaTano
            , ST.quotesAsajjVentress
            , ST.quotesBendu
            , ST.quotesBobaFett
            , ST.quotesC3po
            , ST.quotesCountDooku
            , ST.quotesDarthCaedus
            , ST.quotesDarthVader
            , ST.quotesEmperorPalpatine
            , ST.quotesFinn
            , ST.quotesGrandAdmiralThrawn
            , ST.quotesGrandMoffTarkin
            , ST.quotesGreedo
            , ST.quotesHanSolo
            , ST.quotesJabbaTheHutt
            , ST.quotesJarJarBinks
            , ST.quotesK2so
            , ST.quotesKyloRen
            , ST.quotesLandoCalrissian
            , ST.quotesLeiaOrgana
            , ST.quotesLukeSkywalker
            , ST.quotesMaceWindu
            , ST.quotesMazKanata
            , ST.quotesObiWanKenobi
            , ST.quotesPadmeAmidala
            , ST.quotesQuiGonJinn
            , ST.quotesRey
            , ST.quotesShmiSkywalker
            , ST.quotesYoda
            , ST.alternateCharacterSpellingsAdmiralAckbar
            , ST.alternateCharacterSpellingsAhsokaTano
            , ST.alternateCharacterSpellingsAnakinSkywalker
            , ST.alternateCharacterSpellingsAsajjVentress
            , ST.alternateCharacterSpellingsBendu
            , ST.alternateCharacterSpellingsBobaFett
            , ST.alternateCharacterSpellingsC3po
            , ST.alternateCharacterSpellingsCountDooku
            , ST.alternateCharacterSpellingsDarthCaedus
            , ST.alternateCharacterSpellingsDarthVader
            , ST.alternateCharacterSpellingsEmperorPalpatine
            , ST.alternateCharacterSpellingsFinn
            , ST.alternateCharacterSpellingsGeneralHux
            , ST.alternateCharacterSpellingsGrandAdmiralThrawn
            , ST.alternateCharacterSpellingsGrandMoffTarkin
            , ST.alternateCharacterSpellingsGreedo
            , ST.alternateCharacterSpellingsHanSolo
            , ST.alternateCharacterSpellingsJabbaTheHutt
            , ST.alternateCharacterSpellingsJarJarBinks
            , ST.alternateCharacterSpellingsK2so
            , ST.alternateCharacterSpellingsKyloRen
            , ST.alternateCharacterSpellingsLandoCalrissian
            , ST.alternateCharacterSpellingsLeiaOrgana
            , ST.alternateCharacterSpellingsLukeSkywalker
            , ST.alternateCharacterSpellingsMaceWindu
            , ST.alternateCharacterSpellingsMazKanata
            , ST.alternateCharacterSpellingsObiWanKenobi
            , ST.alternateCharacterSpellingsPadmeAmidala
            , ST.alternateCharacterSpellingsQuiGonJinn
            , ST.alternateCharacterSpellingsRey
            , ST.alternateCharacterSpellingsShmiSkywalker
            , ST.alternateCharacterSpellingsYoda
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie.VForVendetta" $ do
      let functions :: [Fake Text] = [VF.characters, VF.speeches, VF.quotes]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Movie" $ do
      let functions :: [Fake Text] = [MO.quote]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Music.Opera" $ do
      let functions :: [Fake Text] =
            [ OP.italianByGiuseppeVerdi
            , OP.italianByGioacchinoRossini
            , OP.italianByGaetanoDonizetti
            , OP.italianByVincenzoBellini
            ]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Music.Phish" $ do
      let functions :: [Fake Text] = [PH.song]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Music.RockBand" $ do
      let functions :: [Fake Text] = [RO.name]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Music.UmphreysMcgee" $ do
      let functions :: [Fake Text] = [UM.song]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
    it "Music" $ do
      let functions :: [Fake Text] =
            [MU.instruments, MU.bands, MU.albums, MU.genres]
      bools <- verifyFakes functions
      (and bools) `shouldBe` True
