{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE InstanceSigs#-}

module Faker
    (
      Fake(..),
      FakerSettings,
      defaultFakerSettings,
      setLocale,
      setRandomGen,
      getRandomGen,
      FakerException (..),
      fakerLocale,
      Unresolved (..),
      randomVec,
      randomUnresolvedVec,
      operateFields,
      resolveFields,
      resolveUnresolved,
      operateField,
      uncons2,
      interpolateNumbers,
      generate
    ) where

import Control.Exception (Exception)
import Data.Text (Text, strip)
import Data.Typeable
import System.Random (StdGen, split, randomR, newStdGen, mkStdGen)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad (ap)

data FakerSettings = FakerSettings { locale :: Text, randomGen :: StdGen } deriving (Show)

data FakerException = InvalidLocale String
                    | InvalidField String Text
                      deriving (Typeable, Show)

instance Exception FakerException

defaultFakerSettings :: FakerSettings
defaultFakerSettings = FakerSettings { locale = "en", randomGen = (mkStdGen 10000) }

setLocale :: Text -> FakerSettings -> FakerSettings
setLocale localeTxt fs = fs { locale = localeTxt }

setRandomGen :: StdGen -> FakerSettings -> FakerSettings
setRandomGen gen fs = fs { randomGen = gen}

getRandomGen :: FakerSettings -> StdGen
getRandomGen settings = randomGen settings

fakerLocale :: FakerSettings -> Text
fakerLocale FakerSettings {..} = locale

newtype Unresolved a = Unresolved { unresolvedField :: a } deriving (Functor)

instance Applicative Unresolved where
    pure = Unresolved
    Unresolved f1 <*> Unresolved f2 = pure $ f1 f2
instance Monad Unresolved where
    return = pure
    (Unresolved f) >>= f1 = f1 f

randomVec :: (MonadThrow m, MonadIO m) => FakerSettings -> (FakerSettings -> m (Vector Text)) -> m Text
randomVec settings provider = do
  items <- provider settings
  let itemsLen = V.length items
      stdGen = getRandomGen settings
      (index, _) = randomR (0, itemsLen - 1) stdGen
  pure $ items ! index

randomUnresolvedVec :: (MonadThrow m, MonadIO m) => FakerSettings -> (FakerSettings -> m (Unresolved (Vector Text))) -> (FakerSettings -> Text -> m Text) -> m Text
randomUnresolvedVec settings provider resolver = do
  items <- provider settings
  resolveUnresolved settings items resolver

resolveUnresolved :: (MonadThrow m, MonadIO m) => FakerSettings -> Unresolved (Vector Text) -> (FakerSettings -> Text -> m Text) -> m Text
resolveUnresolved settings (Unresolved unres) resolver =
    let unresLen = V.length unres
        stdGen = getRandomGen settings
        (index, _) = randomR (0, unresLen - 1) stdGen
        randomItem = unres ! index
        resolve = if operateField randomItem "hello" == randomItem -- todo: remove hack
                  then interpolateNumbers randomItem
                  else resolver settings randomItem
    in resolve

uncons2 :: Text -> Maybe (String, Text)
uncons2 txt = do
   (c1, rem1) <- T.uncons txt
   (c2, rem2) <- T.uncons rem1
   pure $ ((c1:[c2]), rem2)

-- operateField "#{hello} #{world}" "jam"
-- "jam #{world}"
operateField :: Text -> Text -> Text
operateField origWord word = helper origWord word []
  where
    helper :: Text -> Text -> String -> Text
    helper txt word acc =
      case uncons2 txt of
        Nothing -> origWord
        Just (str, rem) ->
          if str == "#{"
            then let actualRem = dropTillBrace rem
                  in (T.pack acc) <> word <> actualRem
            else case T.uncons txt of
                   Nothing -> origWord
                   Just (c, rem2) -> helper rem2 word (acc <> [c])

operateFields :: Text -> [Text] -> Text
operateFields origWord [] = origWord
operateFields origWord (x:xs) = operateFields (operateField origWord x) xs

dropTillBrace :: Text -> Text
dropTillBrace txt = T.dropWhile (== '}') $ T.dropWhile (/= '}') txt

extractSingleField :: Text -> (Text, Text)
extractSingleField txt = let (field, remaining) = T.span (\x -> x /= '}') txt''
                         in (T.drop 2 field, T.drop 1 remaining)
    where
      txt' = strip txt
      txt'' = snd $ T.span (\x -> x /= '#') txt'

resolveFields :: Text -> [Text]
resolveFields txt = let (field, remaining) = extractSingleField txt
                    in case T.null remaining of
                         True -> [field]
                         False -> [field] <> resolveFields remaining

digitToChar :: Int -> Char
digitToChar 0 = '0'
digitToChar 1 = '1'
digitToChar 2 = '2'
digitToChar 3 = '3'
digitToChar 4 = '4'
digitToChar 5 = '5'
digitToChar 6 = '6'
digitToChar 7 = '7'
digitToChar 8 = '8'
digitToChar 9 = '9'
digitToChar x = error $ "Expected single digit number, but received " <> show x

isHash :: Char -> Bool
isHash c = c == '#'

randomIntText :: (MonadIO m) => m Text -> Char -> m Text
randomIntText acc c = if isHash c
                      then do
                        gen <- liftIO $ newStdGen
                        a <- acc
                        let (int :: Int, _) = randomR (0,9) gen
                        pure $ a <> T.singleton (digitToChar int)
                      else do
                        a <- acc
                        pure $ a <> T.singleton c

-- >> interpolateNumbers "#####"
-- >> 23456
-- >> interpolateNumbers "ab-##"
-- >> ab-32
interpolateNumbers :: (MonadIO m) => Text -> m Text
interpolateNumbers txt = T.foldl' randomIntText (pure T.empty) txt

newtype Fake a = Fake { unFake :: FakerSettings -> IO a }

instance Functor Fake where
    fmap :: (a -> b) -> Fake a -> Fake b
    fmap f (Fake h) = Fake (\r -> do
                              a <- h r
                              let b = f a
                              pure b)

instance Applicative Fake where
    pure x = Fake (\_ -> pure x)
    (<*>) = ap

instance Monad Fake where
    return :: a -> Fake a
    return x = Fake (\_ -> return x)

    (>>=) :: Fake a -> (a -> Fake b) -> Fake b
    (Fake h) >>= k = Fake (\settings ->
                               let stdGen = getRandomGen settings
                                   (r1, _) = split stdGen
                                   m = do
                                     (item :: a) <- h settings
                                     let (Fake k1) = k item
                                     k1 (setRandomGen r1 settings)
                               in m
                         )

generate :: Fake a -> IO a
generate (Fake f) = f defaultFakerSettings

instance MonadIO Fake where
    liftIO :: IO a -> Fake a
    liftIO xs = Fake (\settings -> xs >>= pure)
