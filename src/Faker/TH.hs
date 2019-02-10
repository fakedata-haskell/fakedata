{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.TH where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Language.Haskell.TH

stringTitle :: String -> String
stringTitle [] = []
stringTitle (x:xs) = (toUpper x) : xs

-- λ> runQ [d|state :: Fake Text;state = Fake (resolver stateProvider)|]
-- [SigD state_0 (AppT (ConT Faker.Fake) (ConT Data.Text.Internal.Text)),ValD (VarP state_0) (NormalB (AppE (ConE Faker.Fake) (AppE (VarE Faker.Internal.resolver) (VarE Faker.Provider.Address.stateProvider)))) []]
-- $(genrateFakeField "address" "state")
-- The above splice will generate a function named state
-- Assumes the presence of the function named "addressStateProvider"
generateFakeField :: String -> String -> Q [Dec]
generateFakeField entityName fieldName = do
  let funName = mkName fieldName
      pfn = entityName <> (stringTitle fieldName) <> "Provider"
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
  let funName = mkName fieldName
      pfn = entityName <> (stringTitle fieldName) <> "Provider"
      rfn = "resolve" <> (stringTitle entityName) <> "Text"
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
