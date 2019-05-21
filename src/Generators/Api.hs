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

        (call (unqualifiedReference postgrestImport "Request")
              [call (local "List") [typeAlias]]
        )
        (call
            (unqualifiedReference postgrestImport "get")
            [ string $ C.tableName config
            , record
                [ ( "params"
                  , call
                      (unqualifiedReference postgrestImport "combineParams")
                      [local "defaultParams", local "params"]
                  )
                , ("decoder", unqualifiedReference decoders "decodePlural")
                ]
            ]
        )
    , exposedFunction "post" [("submission", typeAlias)] (request typeAlias)
        $ call
              (unqualifiedReference postgrestImport "post")
              [ string $ C.tableName config
              , record
                  [ ( "body"
                    , call (unqualifiedReference encoders "encode")
                           [local "submission"]
                    )
                  , ("decoder", unqualifiedReference decoders "decodeSingular")
                  ]
              ]
    ]
  where
    request a = call (unqualifiedReference postgrestImport "Request") [a]

    typeAlias = C.tableTypeAlias config

    decoders :: Import
    decoders = import_ (C.decodersModuleReference config) Nothing

    encoders :: Import
    encoders = import_ (C.encodersModuleReference config) Nothing

postgrest = unqualifiedReference postgrestImport

postgrestImport :: Import
postgrestImport = import_ (LocalModule "Api.Postgrest") Nothing

