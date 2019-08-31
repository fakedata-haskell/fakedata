{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.Provider.TH where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Language.Haskell.TH

textTitle :: Text -> Text
textTitle txt =
  case T.uncons txt of
    Nothing -> txt
    Just (c, rem) -> T.cons (toUpper c) rem

-- λ> runQ [d| parseBeerName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a; parseBeerName settings = parseBeerField settings "name"|]
-- [SigD parseBeerName_1 (ForallT [] [AppT (ConT Data.Aeson.Types.FromJSON.FromJSON) (VarT a_0),AppT (ConT GHC.Base.Monoid) (VarT a_0)] (AppT (AppT ArrowT (ConT Faker.FakerSettings)) (AppT (AppT ArrowT (ConT Data.Aeson.Types.Internal.Value)) (AppT (ConT Data.Aeson.Types.Internal.Parser) (VarT a_0))))),FunD parseBeerName_1 [Clause [VarP settings_2] (NormalB (AppE (AppE (VarE Faker.Provider.Beer.parseBeerField) (VarE settings_2)) (LitE (StringL "name")))) []]]
-- λ
-- $(genParser "beer" "name")
-- The above code will produce a function named 'parseBeerName'
-- parseBeerName :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser a
-- Note that a function named parseBeerField should be present in the scope for this to work.
genParser ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> Text -- ^ Field name within the entity.
  -> Q [Dec]
genParser entityName fieldName = do
  let funName =
        mkName $
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <>
        (refinedText $ textTitle fieldName)
  let parserFnName =
        unpack $ "parse" <> (refinedText $ textTitle entityName) <> "Field"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  let tvA = mkName "a"
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [AppT (ConT ''FromJSON) (VarT tvA), AppT (ConT ''Monoid) (VarT tvA)]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (AppT ArrowT (ConT ''Value))
                 (AppT (ConT ''Parser) (VarT tvA)))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE (VarE parserFn) (VarE tsettings))
                  (LitE (StringL (unpack fieldName)))))
            []
        ]
    ]

genParsers ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> [Text] -- ^ Field name within the entity.
  -> Q [Dec]
genParsers entityName fieldName = do
  let fieldNames = map textTitle fieldName
      fieldNames' = refinedText $ T.concat fieldNames
      funName =
        mkName $
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <> (fieldNames')
  let parserFnName =
        unpack $ "parse" <> (refinedText $ textTitle entityName) <> "Fields"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  let tvA = mkName "a"
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [AppT (ConT ''FromJSON) (VarT tvA), AppT (ConT ''Monoid) (VarT tvA)]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (AppT ArrowT (ConT ''Value))
                 (AppT (ConT ''Parser) (VarT tvA)))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE (VarE parserFn) (VarE tsettings))
                  (ListE (map (\x -> LitE (StringL (unpack x))) fieldName))))
            []
        ]
    ]

genProviders ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> [Text] -- ^ Field name within the entity.
  -> Q [Dec]
genProviders entityName fieldName = do
  let fieldNames = map textTitle fieldName
      fieldNames' = refinedText $ T.concat fieldNames
      funName =
        mkName $ unpack $ (refinedText entityName) <> fieldNames' <> "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $ "parse" <> (refinedText $ textTitle entityName) <> fieldNames'
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [ AppT (ConT ''MonadThrow) (VarT tvM)
           , AppT (ConT ''MonadIO) (VarT tvM)
           ]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT (VarT tvM) (AppT (ConT ''Vector) (ConT ''Text)))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE
                     (AppE (VarE 'fetchData) (VarE tsettings))
                     (ConE (mapSource entityName)))
                  (VarE parserFn)))
            []
        ]
    ]

genProvidersSingle ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> [Text] -- ^ Field name within the entity.
  -> Q [Dec]
genProvidersSingle entityName fieldName = do
  let fieldNames = map textTitle fieldName
      fieldNames' = refinedText $ T.concat fieldNames
      funName =
        mkName $ unpack $ (refinedText entityName) <> fieldNames' <> "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $ "parse" <> (refinedText $ textTitle entityName) <> fieldNames'
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [ AppT (ConT ''MonadThrow) (VarT tvM)
           , AppT (ConT ''MonadIO) (VarT tvM)
           ]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT (VarT tvM) (AppT (ConT ''Vector) (ConT ''Text)))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE
                     (AppE (VarE 'fetchDataSingle) (VarE tsettings))
                     (ConE (mapSource entityName)))
                  (VarE parserFn)))
            []
        ]
    ]

-- λ> runQ [d|beerNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text); beerNameProvider settings = fetchData settings Beer parseBeerName|]
-- [SigD beerNameProvider_1 (ForallT [] [AppT (ConT Control.Monad.Catch.MonadThrow) (VarT m_0),AppT (ConT Control.Monad.IO.Class.MonadIO) (VarT m_0)] (AppT (AppT ArrowT (ConT Faker.FakerSettings)) (AppT (VarT m_0) (AppT (ConT Data.Vector.Vector) (ConT Data.Text.Internal.Text))))),FunD beerNameProvider_1 [Clause [VarP settings_2] (NormalB (AppE (AppE (AppE (VarE Config.fetchData) (VarE settings_2)) (ConE Config.Beer)) (VarE Faker.Provider.Beer.parseBeerName))) []]]
-- $(genProvider "beer" "name")
-- This will produce a function named: "beerNameProvider"
-- Assumes the presence of parseBeerName function in the scope.
genProvider ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> Text -- ^ Field name within the entity.
  -> Q [Dec]
genProvider entityName fieldName = do
  let funName =
        mkName $
        unpack $
        (refinedText entityName) <> (refinedText $ textTitle fieldName) <>
        "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <>
        (refinedText $ textTitle fieldName)
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [ AppT (ConT ''MonadThrow) (VarT tvM)
           , AppT (ConT ''MonadIO) (VarT tvM)
           ]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT (VarT tvM) (AppT (ConT ''Vector) (ConT ''Text)))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE
                     (AppE (VarE 'fetchData) (VarE tsettings))
                     (ConE (mapSource entityName)))
                  (VarE parserFn)))
            []
        ]
    ]

-- λ> runQ [d|parseVersionApp :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a); parseVersionApp settings = parseUnresolvedAppField settings "version"|]
-- [SigD parseVersionApp_1 (ForallT [] [AppT (ConT Data.Aeson.Types.FromJSON.FromJSON) (VarT a_0),AppT (ConT GHC.Base.Monoid) (VarT a_0)] (AppT (AppT ArrowT (ConT Faker.FakerSettings)) (AppT (AppT ArrowT (ConT Data.Aeson.Types.Internal.Value)) (AppT (ConT Data.Aeson.Types.Internal.Parser) (AppT (ConT Faker.Internal.Unresolved) (VarT a_0)))))),FunD parseVersionApp_1 [Clause [VarP settings_2] (NormalB (AppE (AppE (VarE Faker.Provider.App.parseUnresolvedAppField) (VarE settings_2)) (LitE (StringL "version")))) []]]
-- $(genParserUnresolved "app" "version")
-- This will generate a function named parseAppVersionUnresolved :: (FromJSON a, Monoid a) => FakerSettings -> Value -> Parser (Unresolved a)
-- This function assumes that parseUnresolvedAppField function is available in the scope.
genParserUnresolved :: Text -> Text -> Q [Dec]
genParserUnresolved entityName fieldName = do
  let funName =
        mkName $
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <>
        (refinedText $ textTitle fieldName) <>
        "Unresolved"
  let parserFnName =
        unpack $
        "parseUnresolved" <> (refinedText $ textTitle entityName) <> "Field"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  let tvA = mkName "a"
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [AppT (ConT ''FromJSON) (VarT tvA), AppT (ConT ''Monoid) (VarT tvA)]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (AppT ArrowT (ConT ''Value))
                 (AppT (ConT ''Parser) (AppT (ConT ''Unresolved) (VarT tvA))))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE (VarE parserFn) (VarE tsettings))
                  (LitE (StringL (unpack fieldName)))))
            []
        ]
    ]

genParserUnresolveds :: Text -> [Text] -> Q [Dec]
genParserUnresolveds entityName fieldNames = do
  let fieldNames' = refinedText $ T.concat $ map textTitle fieldNames
      funName =
        mkName $
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <> fieldNames' <>
        "Unresolved"
  let parserFnName =
        unpack $
        "parseUnresolved" <> (refinedText $ textTitle entityName) <> "Fields"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  let tvA = mkName "a"
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [AppT (ConT ''FromJSON) (VarT tvA), AppT (ConT ''Monoid) (VarT tvA)]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (AppT ArrowT (ConT ''Value))
                 (AppT (ConT ''Parser) (AppT (ConT ''Unresolved) (VarT tvA))))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE (VarE parserFn) (VarE tsettings))
                  (ListE (map (\x -> LitE (StringL (unpack x))) fieldNames))))
            []
        ]
    ]

genProviderUnresolveds ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> [Text] -- ^ Field name within the entity.
  -> Q [Dec]
genProviderUnresolveds entityName fieldNames = do
  let fieldNames' = refinedText $ T.concat $ map textTitle fieldNames
      entityName' = refinedText entityName
      funName = mkName $ unpack $ entityName' <> fieldNames' <> "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $
        "parse" <> (textTitle entityName') <> fieldNames' <> "Unresolved"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [ AppT (ConT ''MonadThrow) (VarT tvM)
           , AppT (ConT ''MonadIO) (VarT tvM)
           ]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (VarT tvM)
                 (AppT (ConT ''Unresolved) (AppT (ConT ''Vector) (ConT ''Text))))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE
                     (AppE (VarE 'fetchData) (VarE tsettings))
                     (ConE (mapSource entityName)))
                  (VarE parserFn)))
            []
        ]
    ]

-- λ> runQ [d|versionAppProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Unresolved (Vector Text)); versionAppProvider settings = fetchData settings App parseVersionApp|]
-- [SigD versionAppProvider_1 (ForallT [] [AppT (ConT Control.Monad.Catch.MonadThrow) (VarT m_0),AppT (ConT Control.Monad.IO.Class.MonadIO) (VarT m_0)] (AppT (AppT ArrowT (ConT Faker.FakerSettings)) (AppT (VarT m_0) (AppT (ConT Faker.Internal.Unresolved) (AppT (ConT Data.Vector.Vector) (ConT Data.Text.Internal.Text)))))),FunD versionAppProvider_1 [Clause [VarP settings_2] (NormalB (AppE (AppE (AppE (VarE Config.fetchData) (VarE settings_2)) (ConE Config.App)) (VarE Faker.Provider.App.parseVersionApp))) []]]
-- $(genProvider "beer" "name")
-- This will produce a function named: "beerNameProvider"
-- Assumes the presence of parseBeerNameUnresolved function in the scope.
genProviderUnresolved ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> Text -- ^ Field name within the entity.
  -> Q [Dec]
genProviderUnresolved entityName fieldName = do
  let funName =
        mkName $
        unpack $ refinedText $ entityName <> (textTitle fieldName) <> "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $
        "parse" <> (refinedText $ textTitle entityName) <>
        (refinedText $ textTitle fieldName) <>
        "Unresolved"
  parserName <- lookupValueName parserFnName
  parserFn <-
    case parserName of
      Nothing -> fail $ "Faker.TH: Didn't find function " <> parserFnName
      Just fn -> return fn
  tsettings <- newName "settings"
  return $
    [ SigD
        funName
        (ForallT
           []
           [ AppT (ConT ''MonadThrow) (VarT tvM)
           , AppT (ConT ''MonadIO) (VarT tvM)
           ]
           (AppT
              (AppT ArrowT (ConT ''FakerSettings))
              (AppT
                 (VarT tvM)
                 (AppT (ConT ''Unresolved) (AppT (ConT ''Vector) (ConT ''Text))))))
    , FunD
        funName
        [ Clause
            [VarP tsettings]
            (NormalB
               (AppE
                  (AppE
                     (AppE (VarE 'fetchData) (VarE tsettings))
                     (ConE (mapSource entityName)))
                  (VarE parserFn)))
            []
        ]
    ]

genAppParser :: Text -> Q [Dec]
genAppParser = genParser "app"

genAppProvider :: Text -> Q [Dec]
genAppProvider = genProvider "app"

genAppParserUnresolved :: Text -> Q [Dec]
genAppParserUnresolved = genParserUnresolved "app"

genAppProviderUnresolved :: Text -> Q [Dec]
genAppProviderUnresolved = genProviderUnresolved "app"
