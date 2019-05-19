module Generators.Types
  ( generate
  )
where

import qualified TableDefinition               as T
import           Elm
import qualified Config                        as C

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile (C.typesModule config) [typeAlias, accessorMap]
 where
  typeAliasName = C.tableTypeAliasName config

  typeAlias :: Expression
  typeAlias =
    TypeAlias Public typeAliasName
      . map columnToField
      . T.columns
      . C.table
      $ config

  accessorMap :: Expression
  accessorMap = RecordDeclaration
    Public
    "map"
    (map columnToMapField . T.columns . C.table $ config)

  columnToField :: T.Column -> Expression
  columnToField col = Field fieldName [fieldType_]
   where
    fieldName  = C.columnFieldName $ T.columnName col
    fieldType_ = T.elmDataTypeToExpression . T.dataType $ col

  r = TypeVariable "r"
  w = TypeVariable "w"

  columnToMapField :: T.Column -> RecordDeclarationField
  columnToMapField col = RecordDeclarationField
    { rdfName      = C.columnFieldName $ T.columnName col
    , rdfSignature = [ Call relation [fieldType_, r, w]
                     , Call relation [LocalReference typeAliasName, r, w]
                     ]
    , rdfValue     =
      Call
        makeOneToOne
        [ LocalReference $ "." ++ fieldName
        , Lambda
          ["r", "f"]
          (UpdateRecord
            "r"
            [ ( fieldName
              , Call (LocalReference "f") [LocalReference $ "r." ++ fieldName]
              )
            ]
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
