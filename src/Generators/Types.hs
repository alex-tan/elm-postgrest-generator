module Generators.Types
  ( generate
  )
where

import qualified TableDefinition               as T
import           Elm
import qualified Config                        as C

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile (C.typesModule config) [typeAlias', accessorMap]
 where
  typeAliasName = C.tableTypeAliasName config

  typeAlias' :: Expression
  typeAlias' = exposedTypeAlias
    typeAliasName
    (map columnToField . T.columns . C.table $ config)

  accessorMap :: Expression
  accessorMap = exposedRecordDeclaration
    "map"
    (map columnToMapField . T.columns . C.table $ config)

  columnToField :: T.Column -> Expression
  columnToField col = field fieldName [fieldType_]
   where
    fieldName  = C.columnFieldName $ T.columnName col
    fieldType_ = T.elmDataTypeToExpression . T.dataType $ col

  r = typeVariable "r"
  w = typeVariable "w"

  columnToMapField :: T.Column -> RecordDeclarationField
  columnToMapField col = RecordDeclarationField
    { rdfName      = C.columnFieldName $ T.columnName col
    , rdfSignature = [ call relation [fieldType_, r, w]
                     , call relation [local typeAliasName, r, w]
                     ]
    , rdfValue     =
      call
        makeOneToOne
        [ local $ "." ++ fieldName
        , lambda
          ["r", "f"]
          (updateRecord
            "r"
            [(fieldName, call (local "f") [local $ "r." ++ fieldName])]
          )
        ]
    }
   where
    fieldName  = C.columnFieldName $ T.columnName col
    fieldType_ = T.elmDataTypeToExpression . T.dataType $ col

relation :: Expression
relation = unqualifiedReference accessors "Relation"

makeOneToOne :: Expression
makeOneToOne = unqualifiedReference accessors "makeOneToOne"

accessors :: Import
accessors =
  import_ (ExternalModule "bChiquet/elm-accessors" "Accessors") Nothing
