{-# OPTIONS_GHC -Wall #-}

module Generators.Decoders
  ( generate
  )
where

import qualified Config                        as C
import           Prelude                           hiding ( maybe )
import qualified TableDefinition               as T
import           Elm
import qualified Generators.Types
import qualified Imports

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile ["Api", C.moduleNamespace config, "Decoders"]
                             [decodeUnit]
 where
  typeAlias' = unqualifiedReference
    (C.importFromGenerator config Generators.Types.generate)
    (C.tableTypeAliasName config)

  decodeUnit = exposedFunction
    "decodeUnit"
    []
    (call jsonDecodeDecoder [typeAlias'])
    (pipeRightChain (call jsonDecodeSucceed [typeAlias'])
                    (concatMap columnToDecoder (T.columns $ C.table config))
    )

jsonDecodeDecoder :: Expression
jsonDecodeDecoder = unqualifiedReference Imports.elmJSONDecode "Decoder"

jsonDecodeSucceed :: Expression
jsonDecodeSucceed = unqualifiedReference Imports.elmJSONDecode "succeed"

columnToDecoder :: T.Column -> [Expression]
columnToDecoder col =
  let decoder = elmDataTypeToDecoder $ T.dataType col
  in  [ call
          (local "|>")
          [ call
              (unqualifiedReference Imports.jsonDecodePipeline "required")
              [ string $ T.columnName col
              , if T.isNullable col then decoder else call maybe [decoder]
              ]
          ]
      ]

maybe :: Expression
maybe = unqualifiedReference Imports.elmJSONDecode "maybe"

elmDataTypeToDecoder :: T.ElmDataType -> Expression
elmDataTypeToDecoder T.String =
  unqualifiedReference Imports.elmJSONDecode "string"
elmDataTypeToDecoder T.Int = unqualifiedReference Imports.elmJSONDecode "int"
elmDataTypeToDecoder T.Float =
  unqualifiedReference Imports.elmJSONDecode "float"
elmDataTypeToDecoder T.Bool = unqualifiedReference Imports.elmJSONDecode "bool"
elmDataTypeToDecoder (T.Unknown _) = local "unknownDecode"
elmDataTypeToDecoder T.Posix =
  unqualifiedReference Imports.jsonDecodeExtra "datetime"
elmDataTypeToDecoder T.Date = local "unknownDateDecoder"
