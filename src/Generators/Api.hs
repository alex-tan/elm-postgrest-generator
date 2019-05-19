module Generators.Api
    ( generate
    )
where

import qualified Config                        as C
import           Prelude                           hiding ( maybe )
import           Elm

generate :: C.TableConfig -> ModuleFile
generate config =
    let typeAlias = C.tableTypeAlias config
        sig       = Call (LocalReference "AP.Request")
                         [Call (LocalReference "List") [typeAlias]]
        functionName = "getPlural"
    in  ModuleFile ["Api", C.moduleNamespace config]
                   [exposedFunction sig functionName []]
