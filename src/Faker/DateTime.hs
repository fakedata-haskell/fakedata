module Faker.DateTime where

import Control.Monad.IO.Class (liftIO)

-- | @since 0.2.0
import Data.Text
import Data.Time
import Faker
import Faker.Combinators

-- | Fake 'UTCTime' between 17-11-1858 and the current time
utc :: Fake UTCTime
utc = do
  now <- liftIO getCurrentTime
  utcBetween
    (UTCTime
       { utctDay = (ModifiedJulianDay {toModifiedJulianDay = 0})
       , utctDayTime = 0
       })
    now

-- | Fake 'Day' between 17-11-1858 and the current day
day :: Fake Day
day = do
  now <- liftIO getCurrentTime
  dayBetween (ModifiedJulianDay {toModifiedJulianDay = 0}) (utctDay now)

-- | Generates a random UTCTime in the range [from, to].
utcBetween :: UTCTime -> UTCTime -> Fake UTCTime
utcBetween from to = do
  delta <- fromRange (0 :: Double, realToFrac $ diffUTCTime to from)
  return $ addUTCTime (realToFrac delta) from

-- | Generates a random Day in the range [from, to].
dayBetween :: Day -> Day -> Fake Day
dayBetween from to = do
  delta <- fromRange (0, diffDays to from)
  return $ addDays delta from

-- | Generates a random Day in the year range [from, to].
dayBetweenYears :: Integer -> Integer -> Fake Day
dayBetweenYears ystart yend =
  fakeEnumFromTo (fromGregorian ystart 1 1) (fromGregorian yend 12 31)

timeBetweenHours :: Int -> Int -> Fake DiffTime
timeBetweenHours hstart hend =
  secondsToDiffTime <$> fromRange (fromIntegral from, fromIntegral to)
  where
    from = hstart * 3600
    to = hend * 3599

utcBetweenYears :: Integer -> Integer -> Fake UTCTime
utcBetweenYears ystart yend =
  UTCTime <$> dayBetweenYears ystart yend <*> timeBetweenHours 0 24
