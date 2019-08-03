{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.TH
  ( generateFakeField
  , generateFakeFields
  , generateFakeFieldUnresolved
  , generateFakeFieldUnresolveds
  , generateFakeField2
  ) where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char (toLower, toUpper)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Language.Haskell.TH

stringTitle :: String -> String
stringTitle [] = []
stringTitle (x:xs) = (toUpper x) : xs

lowerTitle :: String -> String
lowerTitle [] = []
lowerTitle (x:xs) = (toLower x) : xs

-- λ> runQ [d|state :: Fake Text;state = Fake (resolver stateProvider)|]
-- [SigD state_0 (AppT (ConT Faker.Fake) (ConT Data.Text.Internal.Text)),ValD (VarP state_0) (NormalB (AppE (ConE Faker.Fake) (AppE (VarE Faker.Internal.resolver) (VarE Faker.Provider.Address.stateProvider)))) []]
-- $(genrateFakeField "address" "state")
-- The above splice will generate a function named state
-- Assumes the presence of the function named "addressStateProvider"
generateFakeField :: String -> String -> Q [Dec]
generateFakeField entityName fieldName = do
  let funName =
        mkName $
        case refinedString fieldName of
          "type" -> "type'"
          other -> other
      pfn = refinedString $ entityName <> (stringTitle fieldName) <> "Provider"
  providerName <- lookupValueName pfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $ "generateFakefield: Function " <> pfn <> " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB (AppE (ConE 'Fake) (AppE (VarE 'resolver) (VarE providerFn))))
        []
    ]

-- todo: replace this with above one
generateFakeField2 :: String -> String -> Q [Dec]
generateFakeField2 entityName fieldName = do
  let funName =
        mkName $
        case refinedString fieldName of
          "type" -> "type'"
          other -> other
      pfn = refinedString $ entityName <> (stringTitle fieldName) <> "Provider"
  providerName <- lookupValueName pfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $ "generateFakefield: Function " <> pfn <> " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB
           (AppE
              (ConE 'Fake)
              (AppE
                 (AppE
                    (AppE (VarE 'cachedRandomVec) (LitE (StringL entityName)))
                    (LitE (StringL fieldName)))
                 (VarE providerFn))))
        []
    ]

generateFakeFields2 :: String -> [String] -> Q [Dec]
generateFakeFields2 entityName fieldName = do
  let fieldName' = map stringTitle fieldName
      fieldName'' = concat fieldName'
      pfn = refinedString $ entityName <> fieldName'' <> "Provider"
      funNameString = lowerTitle $ refinedString fieldName''
      funName = mkName funNameString
  providerName <- lookupValueName pfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $ "generateFakefield: Function " <> pfn <> " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB
           (AppE
              (ConE 'Fake)
              (AppE
                 (AppE
                    (AppE (VarE 'cachedRandomVec) (LitE (StringL entityName)))
                    (LitE (StringL funNameString)))
                 (VarE providerFn))))
        []
    ]

generateFakeFields :: String -> [String] -> Q [Dec]
generateFakeFields entityName fieldName = do
  let fieldName' = map stringTitle fieldName
      fieldName'' = concat fieldName'
      pfn = refinedString $ entityName <> fieldName'' <> "Provider"
      funName = mkName $ lowerTitle $ refinedString fieldName''
  providerName <- lookupValueName pfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $ "generateFakefield: Function " <> pfn <> " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB (AppE (ConE 'Fake) (AppE (VarE 'resolver) (VarE providerFn))))
        []
    ]

-- λ> runQ [d|city2 :: Fake Text;city2 = Fake (unresolvedResolver streetNameProvider resolveAddressText)|]
-- [SigD city2_0 (AppT (ConT Faker.Fake) (ConT Data.Text.Internal.Text)),ValD (VarP city2_0) (NormalB (AppE (ConE Faker.Fake) (AppE (AppE (VarE Faker.Internal.unresolvedResolver) (VarE Faker.Provider.Address.streetNameProvider)) (VarE Faker.Provider.Address.resolveAddressText)))) []]
-- $(genrateFakeField "address" "state")
-- The above splice will generate a function named state
-- Assumes the presence of the function named "addressStateProvider" and "resolveAddressText"
generateFakeFieldUnresolved :: String -> String -> Q [Dec]
generateFakeFieldUnresolved entityName fieldName = do
  let funName = mkName $ refinedString fieldName
      pfn = refinedString $ entityName <> (stringTitle fieldName) <> "Provider"
      rfn = "resolve" <> (refinedString $ stringTitle entityName) <> "Text"
  providerName <- lookupValueName pfn
  resolverName <- lookupValueName rfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $
        "generateFakeFieldUnresolved: Function " <> pfn <> " not found in scope"
      Just fn -> return fn
  resolverFn <-
    case resolverName of
      Nothing ->
        fail $
        "generateFakeFieldUnresolved: Function " <> rfn <> " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB
           (AppE
              (ConE 'Fake)
              (AppE
                 (AppE (VarE 'unresolvedResolver) (VarE providerFn))
                 (VarE resolverFn))))
        []
    ]

generateFakeFieldUnresolveds :: String -> [String] -> Q [Dec]
generateFakeFieldUnresolveds entityName fieldNames = do
  let fieldName' = refinedString $ concat $ map stringTitle fieldNames
      funName = mkName $ lowerTitle fieldName'
      pfn = refinedString $ entityName <> fieldName' <> "Provider"
      rfn = "resolve" <> (refinedString $ stringTitle entityName) <> "Text"
  providerName <- lookupValueName pfn
  resolverName <- lookupValueName rfn
  providerFn <-
    case providerName of
      Nothing ->
        fail $
        "generateFakeFieldUnresolveds: Function " <> pfn <>
        " not found in scope"
      Just fn -> return fn
  resolverFn <-
    case resolverName of
      Nothing ->
        fail $
        "generateFakeFieldUnresolveds: Function " <> rfn <>
        " not found in scope"
      Just fn -> return fn
  return $
    [ SigD funName (AppT (ConT ''Fake) (ConT ''Text))
    , ValD
        (VarP funName)
        (NormalB
           (AppE
              (ConE 'Fake)
              (AppE
                 (AppE (VarE 'unresolvedResolver) (VarE providerFn))
                 (VarE resolverFn))))
        []
    ]
