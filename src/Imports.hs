module Imports where

import           Elm

postgrestClient :: Import
postgrestClient = importAliased
  (ExternalModule "alex-tan/postgrest-client" ["Postgrest", "Client"])
  "P"

elmJSONDecode :: Import
elmJSONDecode = importUnaliased (ExternalModule "elm/json" ["Json", "Decode"])

jsonDecodePipeline :: Import
jsonDecodePipeline = importUnaliased
  (ExternalModule "NoRedInk/elm-json-decode-pipeline"
                  ["Json", "Decode", "Pipeline"]
  )

jsonDecodeExtra :: Import
jsonDecodeExtra = importUnaliased
  (ExternalModule "elm-community/json-extra" ["Json", "Decode", "Extra"])

elmTime :: Import
elmTime = importUnaliased (ExternalModule "elm/time" ["Time"])

date :: Import
date = importUnaliased (ExternalModule "justinmimbs/date" ["Date"])

elmAccessors :: Import
elmAccessors =
  importUnaliased (ExternalModule "bChiquet/elm-accessors" ["Accessors"])
