{-#LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor #-}

module Faker.Internal where

import qualified Data.Text as T
import Data.Text (Text, strip)
import Faker
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Control.Monad.Catch
import System.Random

newtype Unresolved a = Unresolved
  { unresolvedField :: a
  } deriving (Functor)

instance Applicative Unresolved where
  pure = Unresolved
  Unresolved f1 <*> Unresolved f2 = pure $ f1 f2

instance Monad Unresolved where
  return = pure
  (Unresolved f) >>= f1 = f1 f

randomVec ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m (Vector Text))
  -> m Text
randomVec settings provider = do
  items <- provider settings
  let itemsLen = V.length items
      stdGen = getRandomGen settings
      (index, _) = randomR (0, itemsLen - 1) stdGen
  if itemsLen == 0
    then throwM $ NoDataFound settings
    else pure $ items ! index

randomUnresolvedVec ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> Text -> m Text)
  -> m Text
randomUnresolvedVec settings provider resolver = do
  items <- provider settings
  resolveUnresolved settings items resolver

resolveUnresolved ::
     (MonadThrow m, MonadIO m)
  => FakerSettings
  -> Unresolved (Vector Text)
  -> (FakerSettings -> Text -> m Text)
  -> m Text
resolveUnresolved settings (Unresolved unres) resolver =
  let unresLen = V.length unres
      stdGen = getRandomGen settings
      (index, _) = randomR (0, unresLen - 1) stdGen
      randomItem = unres ! index
      resolve = if operateField randomItem "hello" == randomItem -- todo: remove hack
                then pure $ interpolateNumbers settings randomItem
                else resolver settings randomItem
  in if unresLen == 0
     then throwM $ NoDataFound settings
     else resolve

uncons2 :: Text -> Maybe (String, Text)
uncons2 txt = do
  (c1, rem1) <- T.uncons txt
  (c2, rem2) <- T.uncons rem1
  pure $ ((c1 : [c2]), rem2)

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
extractSingleField txt =
  let (field, remaining) = T.span (\x -> x /= '}') txt''
   in (T.drop 2 field, T.drop 1 remaining)
  where
    txt' = strip txt
    txt'' = snd $ T.span (\x -> x /= '#') txt'

resolveFields :: Text -> [Text]
resolveFields txt =
  let (field, remaining) = extractSingleField txt
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

-- >> interpolateNumbers "#####"
-- >> 23456
-- >> interpolateNumbers "ab-##"
-- >> ab-32
interpolateNumbers :: FakerSettings -> Text -> Text
interpolateNumbers settings txt = helper settings [] txt
  where
    helper settings acc txt =
      case T.null txt of
        True -> T.pack acc
        False ->
          case T.uncons txt of
            Nothing -> T.pack acc
            Just (c, rem) ->
              if isHash c
                then let stdGen = getRandomGen settings
                         (int, gen) = randomR (0, 9) stdGen
                      in helper
                           (setRandomGen gen settings)
                           (acc ++ [digitToChar int])
                           rem
                else helper settings (acc ++ [c]) rem

resolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Vector Text))
  -> FakerSettings
  -> m Text
resolver provider = \settings -> randomVec settings provider

unresolvedResolver ::
     (MonadThrow m, MonadIO m)
  => (FakerSettings -> m (Unresolved (Vector Text)))
  -> (FakerSettings -> Text -> m Text)
  -> (FakerSettings -> m Text)
unresolvedResolver provider resolver =
  \settings -> randomUnresolvedVec settings provider resolver
