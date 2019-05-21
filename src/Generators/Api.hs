module Generators.Api
    ( generate
    )
where

import qualified Config                        as C
import           Elm

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile
    ["Api", C.moduleNamespace config]
    [ exposedFunction
        "getPlural"
        [("params", unqualifiedReference postgrestImport "Params")]

        (Call (unqualifiedReference postgrestImport "Request")
              [postgrest "Params", Call (LocalReference "List") [typeAlias]]
        )
        (Call
            (unqualifiedReference postgrestImport "get")
            [ Str $ C.tableName config
            , Record
                [ ( "params"
                  , Call
                      (unqualifiedReference postgrestImport "combineParams")
                      [LocalReference "defaultParams", LocalReference "params"]
                  )
                , ("decoder", unqualifiedReference decoders "decodePlural")
                ]
            ]
        )
    , exposedFunction "post" [("submission", typeAlias)] (request typeAlias)
        $ Call
              (unqualifiedReference postgrestImport "post")
              [ Str $ C.tableName config
              , Record
                  [ ( "body"
                    , Call
                        (unqualifiedReference encoders "encode")
                        [ LocalReference "defaultParams"
                        , LocalReference "params"
                        ]
                    )
                  , ("decoder", unqualifiedReference decoders "decodeSingular")
                  ]
              ]
    ]
  where
    request a = Call (unqualifiedReference postgrestImport "Request") [a]

    typeAlias = C.tableTypeAlias config

    decoders :: Import
    decoders = import_ (C.decodersModuleReference config) Nothing

    encoders :: Import
    encoders = import_ (C.encodersModuleReference config) Nothing

postgrest = unqualifiedReference postgrestImport

postgrestImport :: Import
postgrestImport = import_ (LocalModule "Api.Postgrest") Nothing

