module Generators.Decoders
  ( generate
  )
where

import qualified Config                        as C
import           Prelude                           hiding ( maybe )
import qualified TableDefinition               as T
import           Elm                           as E

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile ["Api", C.moduleNamespace config, "Decoders"]
                             [decodePlural, decodeSingular, decodeUnit]
 where
  typeAlias    = C.tableTypeAlias config

  decodePlural = exposedFunction
    (Call jsonDecodeDecoder [typeAlias])
    "decodePlural"
    [ Call (unqualifiedReference jsonDecode "list")
           [LocalReference "decodeUnit"]
    ]

  decodeSingular = exposedFunction
    (Call jsonDecodeDecoder [typeAlias])
    "decodeSingular"
    [ Call (unqualifiedReference jsonDecode "index")
           [E.Int_ 0, LocalReference "decodeUnit"]
    ]

  decodeUnit = exposedFunction
    (Call jsonDecodeDecoder [typeAlias])
    "decodeUnit"
    ( Call jsonDecodeSucceed [typeAlias]
    : concatMap columnToDecoder (T.columns $ C.table config)
    )

jsonDecodeDecoder :: Expression
jsonDecodeDecoder = unqualifiedReference jsonDecode "Decoder"

jsonDecodeSucceed :: Expression
jsonDecodeSucceed = unqualifiedReference jsonDecode "succeed"

columnToDecoder :: T.Column -> [Expression]
columnToDecoder col =
  let decoder = elmDataTypeToDecoder $ T.dataType col
  in  [ Call
          (LocalReference "|>")
          [ unqualifiedReference jsonDecodePipeline "required"
          , Str $ T.columnName col
          , if T.isNullable col then decoder else Call maybe [decoder]
          ]
      ]

maybe :: Expression
maybe = unqualifiedReference jsonDecode "maybe"

elmDataTypeToDecoder :: T.ElmDataType -> Expression
elmDataTypeToDecoder T.String      = unqualifiedReference jsonDecode "string"
elmDataTypeToDecoder T.Int         = unqualifiedReference jsonDecode "int"
elmDataTypeToDecoder T.Float       = unqualifiedReference jsonDecode "float"
elmDataTypeToDecoder T.Bool        = unqualifiedReference jsonDecode "bool"
elmDataTypeToDecoder (T.Unknown _) = LocalReference "unknownDecode"
elmDataTypeToDecoder T.Posix = unqualifiedReference jsonDecodeExtra "datetime"
elmDataTypeToDecoder T.Date        = LocalReference "unknownDateDecoder"

jsonDecodePipeline :: Import
jsonDecodePipeline = import_
  (ExternalModule "NoRedInk/elm-json-decode-pipeline" "Json.Decode.Pipeline")
  Nothing

jsonDecode :: Import
jsonDecode = import_ (ExternalModule "elm/json" "Json.Decode") Nothing

jsonDecodeExtra :: Import
jsonDecodeExtra = import_
  (ExternalModule "elm-community/json-extra" "Json.Decode.Extra")
  Nothing
