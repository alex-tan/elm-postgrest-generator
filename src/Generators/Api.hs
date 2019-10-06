module Generators.Api
    ( generate
    )
where

import qualified Config                        as C
import           Elm
import qualified Generators.Decoders
import qualified Generators.Encoders
import qualified Generators.Types

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile
    ["Api", C.moduleNamespace config]
    [ exposedFunction
        "getPlural"
        [("params", postgrestReference "Params")]
        (call (postgrestReference "Request") [call (local "List") [typeAlias']])
        (call
            (postgrestReference "getMany")
            [ string endpoint
            , record
                [ ( "params"
                  , call (postgrestReference "combineParams")
                         [local "defaultParams", local "params"]
                  )
                , ("decoder", decoderReference "decodePlural")
                ]
            ]
        )
    , exposedFunction "post" [("submission", typeAlias')] (request typeAlias')
        $ call
              (postgrestReference "post")
              [ string endpoint
              , record
                  [ ( "body"
                    , call (encoderReference "encode") [local "submission"]
                    )
                  , ("decoder", decoderReference "decodeSingular")
                  ]
              ]
    , exposedFunction "endpoint" [] (postgrestEndpoint typeAlias')
        $ call (postgrestReference "endpoint") [string endpoint, decodeUnit]
    ]
  where
    endpoint   = C.apiPrefix config ++ C.tableName config

    decodeUnit = unqualifiedReference decoders "decodeSingular"

    typeAlias' = unqualifiedReference
        (C.importFromGenerator config $ Generators.Types.generate config)
        (C.tableTypeAliasName config)

    request a = call (postgrestReference "Request") [a]

    postgrestEndpoint a = call (postgrestReference "Endpoint") [a]

    decoders :: Import
    decoders =
        C.importFromGenerator config $ Generators.Decoders.generate config

    encoders :: Import
    encoders =
        C.importFromGenerator config $ Generators.Encoders.generate config

    decoderReference = unqualifiedReference decoders
    encoderReference = unqualifiedReference encoders


postgrestReference :: FunctionName -> Expression
postgrestReference = qualifiedReference postgrestImport

postgrestImport :: Import
postgrestImport = import_ postgrestClient (Just "P")

postgrestClient :: Module
postgrestClient =
    ExternalModule "alex-tan/postgrest-client" ["Postgrest", "Client"]

