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
        sig       = Call (unqualifiedReference postgrestImport "Request")
                         [Call (LocalReference "List") [typeAlias]]
    in  
        -- typeAlias    = C.tableTypeAlias config
    
        ModuleFile ["Api", C.moduleNamespace config]
                   [ exposedFunction sig "getPlural" 
                   $ Call ( unqualifiedReference postgrestImport "get" ) [
                       Record 
                           [ ("params", LocalReference "params")
                           , ("decoder", LocalReference "decoder")
                           ]
                   ]
                   ]
    -- AP.get "commit_idf_uploads"
    --     { params = params
    --     , decoder = decodePlural
    --     }

postgrestImport :: Import
postgrestImport = import_ (LocalModule "Api.Postgrest") Nothing