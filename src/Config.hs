{-# LANGUAGE NamedFieldPuns #-}

module Config
  ( TableConfig(..)
  , tableTypeAlias
  , tableTypeAliasName
  , tableModule
  , columnFieldName
  , moduleNamespace
  , typesModule
  )
where

import           Elm                                      ( Expression
                                                          , Module(LocalModule)
                                                          , unqualifiedReference
                                                          , import_
                                                          , Import
                                                          )
import qualified TableDefinition               as T
import           Data.Char                     as Char
import           Data.List                                ( intercalate )
import           Data.Maybe                               ( fromMaybe )
import           Text.Casing                              ( fromAny
                                                          , toCamel
                                                          )

data TableConfig = TableConfig
    { specifiedTypeAlias :: Maybe String
    , specifiedModuleNamespace :: Maybe String
    , table :: T.Table
    }

typesModule :: TableConfig -> [String]
typesModule config = ["Api", moduleNamespace config, "Types"]

moduleNamespace :: TableConfig -> String
moduleNamespace c =
  fromMaybe (tableModule . T.name . table $ c) (specifiedModuleNamespace c)

tableTypeAlias :: TableConfig -> Expression
tableTypeAlias config =
  unqualifiedReference (typesModuleImport config) $ tableTypeAliasName config

tableTypeAliasName :: TableConfig -> String
tableTypeAliasName TableConfig { specifiedTypeAlias, table } =
  fromMaybe (capitalized . toCamelCase $ T.name table) specifiedTypeAlias

tableModule :: String -> String
tableModule = capitalized . toCamelCase

columnFieldName :: String -> String
columnFieldName = toCamelCase

toCamelCase :: String -> String
toCamelCase = toCamel . fromAny

capitalized :: String -> String
capitalized (x : xs) = Char.toUpper x : xs
capitalized []       = []

typesModuleImport :: TableConfig -> Import
typesModuleImport config =
  import_ (LocalModule $ (intercalate "." . typesModule) config) Nothing
