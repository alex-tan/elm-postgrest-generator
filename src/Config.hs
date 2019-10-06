{-# LANGUAGE NamedFieldPuns #-}

module Config
  ( TableConfig(..)
  , tableTypeAliasName
  , importFromGenerator
  , tableModule
  , columnFieldName
  , moduleNamespace
  , tableName
  )
where

import           Elm                                      ( Expression
                                                          , Module(LocalModule)
                                                          , unqualifiedReference
                                                          , import_
                                                          , Import
                                                          , moduleNameParts
                                                          , ModuleFile
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
    , sourceDirectory :: String
    , apiPrefix :: String
    } deriving (Show)

tableName = T.name . table

moduleNamespace :: TableConfig -> String
moduleNamespace c =
  fromMaybe (tableModule . T.name . table $ c) (specifiedModuleNamespace c)

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

importFromGenerator :: TableConfig -> (TableConfig -> ModuleFile) -> Import
importFromGenerator config generator =
  let module_ :: Module
      module_ = (LocalModule . moduleNameParts) (generator config)
  in  import_ module_ Nothing
