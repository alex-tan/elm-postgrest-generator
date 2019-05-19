module Generators.Types
  ( generate
  )
where

import qualified TableDefinition               as T
import           Elm
import qualified Config                        as C

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile (C.typesModule config) [typeAlias]
 where
  typeAlias :: Expression
  typeAlias =
    let typeAliasName = C.tableTypeAliasName config
    in  TypeAlias Public typeAliasName
          . map columnToField
          . T.columns
          . C.table
          $ config

columnToField :: T.Column -> Expression
columnToField col = Field fieldName [fieldType_]
 where
  fieldName  = C.columnFieldName $ T.columnName col
  fieldType_ = T.elmDataTypeToExpression . T.dataType $ col
