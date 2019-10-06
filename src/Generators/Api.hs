{-# LANGUAGE NamedFieldPuns #-}
module Generators.Api
    ( generate
    )
where

import qualified Imports
import qualified Config                        as C
import           Elm
import qualified Generators.Decoders
import qualified Generators.Encoders
import qualified Generators.Types

generate :: C.TableConfig -> ModuleFile
generate config = ModuleFile
    ["Api", C.moduleNamespace config]
    [getMany context, post context, endpoint context, url config]
  where
    decoders :: Import
    decoders = C.importFromGenerator config Generators.Decoders.generate

    encoders :: Import
    encoders = C.importFromGenerator config Generators.Encoders.generate

    context  = Context
        { typeAlias'       = unqualifiedReference
                                 (C.importFromGenerator config
                                                        Generators.Types.generate
                                 )
                                 (C.tableTypeAliasName config)
        , decoderReference = unqualifiedReference decoders
        , encoderReference = unqualifiedReference encoders
        }

postgrestReference :: FunctionName -> Expression
postgrestReference = qualifiedReference Imports.postgrestClient

data Context = Context
    { typeAlias' :: Expression
    , decoderReference :: FunctionName -> Expression
    , encoderReference :: FunctionName -> Expression
    }

getMany :: Context -> Expression
getMany Context { typeAlias', decoderReference } = exposedFunction
    "getMany"
    []
    (call (postgrestReference "Request") [call (local "List") [typeAlias']])
    (call (postgrestReference "getMany") [local "endpoint", local "primaryKey"])

post :: Context -> Expression
post Context { typeAlias', decoderReference, encoderReference } =
    let request a = call (postgrestReference "Request") [a]
    in  exposedFunction "post" [("submission", typeAlias')] (request typeAlias')
            $ call
                  (postgrestReference "post")
                  [ local "url"
                  , record
                      [ ( "body"
                        , call (encoderReference "encode") [local "submission"]
                        )
                      , ("decoder", decoderReference "decodeSingular")
                      ]
                  ]

endpoint :: Context -> Expression
endpoint Context { typeAlias', decoderReference } =
    let decodeUnit = decoderReference "decodeSingular"
        postgrestEndpoint a = call (postgrestReference "Endpoint") [a]
    in  exposedFunction "endpoint" [] (postgrestEndpoint typeAlias')
            $ call (postgrestReference "endpoint") [local "url", decodeUnit]


url :: C.TableConfig -> Expression
url config = unexposedFunction
    "url"
    []
    (local "String")
    (string $ C.apiPrefix config ++ "/" ++ C.tableName config)
