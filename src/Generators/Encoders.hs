{-# OPTIONS_GHC -Wall #-}

module Generators.Encoders
  ( generate
  )
where

import qualified Config                        as C
import           Prelude                           hiding ( maybe )
import qualified TableDefinition               as T
import           Elm
import qualified Generators.Types

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile ["Api", C.moduleNamespace config, "Encoders"]
                             [decodePlural, decodeSingular, decodeUnit]
 where
  typeAlias' = unqualifiedReference
    (C.importFromGenerator config $ Generators.Types.generate config)
    (C.tableTypeAliasName config)

  decodePlural = exposedFunction
    "decodePlural"
    []
    (call jsonDecodeDecoder [call (local "List") [typeAlias']])
    (call (unqualifiedReference jsonDecode "list") [decodeUnitReference])

  decodeSingular = exposedFunction
    "decodeSingular"
    []
    (call jsonDecodeDecoder [typeAlias'])
    (call (unqualifiedReference jsonDecode "index") [int 0, decodeUnitReference]
    )

  decodeUnit = exposedFunction
    "decodeUnit"
    []
    (call jsonDecodeDecoder [typeAlias'])
    (pipeRightChain (call jsonDecodeSucceed [typeAlias'])
                    (concatMap columnToDecoder (T.columns $ C.table config))
    )

decodeUnitReference :: Expression
decodeUnitReference = local "decodeUnit"

jsonDecodeDecoder :: Expression
jsonDecodeDecoder = unqualifiedReference jsonDecode "Decoder"

jsonDecodeSucceed :: Expression
jsonDecodeSucceed = unqualifiedReference jsonDecode "succeed"

columnToDecoder :: T.Column -> [Expression]
columnToDecoder col =
  let decoder = elmDataTypeToDecoder $ T.dataType col
  in  [ call
          (local "|>")
          [ call
              (unqualifiedReference jsonDecodePipeline "required")
              [ string $ T.columnName col
              , if T.isNullable col then decoder else call maybe [decoder]
              ]
          ]
      ]

maybe :: Expression
maybe = unqualifiedReference jsonDecode "maybe"

elmDataTypeToDecoder :: T.ElmDataType -> Expression
elmDataTypeToDecoder T.String      = unqualifiedReference jsonDecode "string"
elmDataTypeToDecoder T.Int         = unqualifiedReference jsonDecode "int"
elmDataTypeToDecoder T.Float       = unqualifiedReference jsonDecode "float"
elmDataTypeToDecoder T.Bool        = unqualifiedReference jsonDecode "bool"
elmDataTypeToDecoder (T.Unknown _) = local "unknownDecode"
elmDataTypeToDecoder T.Posix = unqualifiedReference jsonDecodeExtra "datetime"
elmDataTypeToDecoder T.Date        = local "unknownDateDecoder"

jsonDecodePipeline :: Import
jsonDecodePipeline = import_
  (ExternalModule "NoRedInk/elm-json-decode-pipeline"
                  ["Json", "Decode", "Pipeline"]
  )
  Nothing

jsonDecode :: Import
jsonDecode = import_ (ExternalModule "elm/json" ["Json", "Decode"]) Nothing

jsonDecodeExtra :: Import
jsonDecodeExtra = import_
  (ExternalModule "elm-community/json-extra" ["Json", "Decode", "Extra"])
  Nothing
