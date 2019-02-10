{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Faker.Provider.TH where

import Config
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Text (Text, toTitle, unpack)
import Data.Vector (Vector)
import Data.Yaml
import Faker
import Faker.Internal
import Language.Haskell.TH

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
        mkName $ unpack $ "parse" <> (toTitle entityName) <> (toTitle fieldName)
  let parserFnName = unpack $ "parse" <> (toTitle entityName) <> "Field"
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

mapSource :: Text -> Name
mapSource "beer" = 'Beer

-- λ> runQ [d|beerNameProvider :: (MonadThrow m, MonadIO m) => FakerSettings -> m (Vector Text); beerNameProvider settings = fetchData settings Beer parseBeerName|]
-- [SigD beerNameProvider_1 (ForallT [] [AppT (ConT Control.Monad.Catch.MonadThrow) (VarT m_0),AppT (ConT Control.Monad.IO.Class.MonadIO) (VarT m_0)] (AppT (AppT ArrowT (ConT Faker.FakerSettings)) (AppT (VarT m_0) (AppT (ConT Data.Vector.Vector) (ConT Data.Text.Internal.Text))))),FunD beerNameProvider_1 [Clause [VarP settings_2] (NormalB (AppE (AppE (AppE (VarE Config.fetchData) (VarE settings_2)) (ConE Config.Beer)) (VarE Faker.Provider.Beer.parseBeerName))) []]]
genProvider ::
     Text -- ^ Entity name. Example: animal, beer etc. This should be always lowercase.
  -> Text -- ^ Field name within the entity.
  -> Q [Dec]
genProvider entityName fieldName = do
  let funName =
        mkName $ unpack $ entityName <> (toTitle fieldName) <> "Provider"
      tvM = mkName "m"
      parserFnName =
        unpack $ "parse" <> (toTitle entityName) <> (toTitle fieldName)
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

genParserAndProvider :: Text -> Text -> Q [Dec]
genParserAndProvider entityName fieldName = do
  dec1 <- genParser entityName fieldName
  dec2 <- genProvider entityName fieldName
  return $ dec1 <> dec2
-- Interesting testing strategy: https://stackoverflow.com/questions/30655883/how-can-i-automate-testing-a-template-haskell-function#comment49414077_30655883
