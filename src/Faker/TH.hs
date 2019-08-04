{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.TH
  ( generateFakeField
  , generateFakeFields
  , generateFakeFieldUnresolved
  , generateFakeFieldUnresolveds
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

generateFakeFields :: String -> [String] -> Q [Dec]
generateFakeFields entityName fieldName = do
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

-- λ> runQ runQ [d|community :: Fake Text; community = Fake (cachedRandomUnresolvedVec "address" "community" communityProvider resolveAddressField)|]
-- [SigD community_1 (AppT (ConT Faker.Fake) (ConT Data.Text.Internal.Text)),ValD (VarP community_1) (NormalB (AppE (ConE Faker.Fake) (AppE (AppE (AppE (AppE (VarE Faker.Internal.cachedRandomUnresolvedVec) (LitE (StringL "address"))) (LitE (StringL "community"))) (VarE Faker.Provider.Address.communityProvider)) (VarE Faker.Provider.Address.resolveAddressField)))) []]
-- $(genrateFakeFieldUnresolved "address" "comunity")
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
                 (AppE
                    (AppE
                       (AppE
                          (VarE 'cachedRandomUnresolvedVec)
                          (LitE (StringL entityName)))
                       (LitE (StringL fieldName)))
                    (VarE providerFn))
                 (VarE resolverFn))))
        []
    ]

generateFakeFieldUnresolveds :: String -> [String] -> Q [Dec]
generateFakeFieldUnresolveds entityName fieldNames = do
  let fieldName' = refinedString $ concat $ map stringTitle fieldNames
      funNameString = lowerTitle fieldName'
      funName = mkName funNameString
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
                 (AppE
                    (AppE
                       (AppE
                          (VarE 'cachedRandomUnresolvedVec)
                          (LitE (StringL entityName)))
                       (LitE (StringL funNameString)))
                    (VarE providerFn))
                 (VarE resolverFn))))
        []
    ]
