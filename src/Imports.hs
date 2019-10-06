module Imports where

import           Elm

postgrestClient :: Import
postgrestClient = import_
  (ExternalModule "alex-tan/postgrest-client" ["Postgrest", "Client"])
  Nothing

elmJSONDecode :: Import
elmJSONDecode = import_ (ExternalModule "elm/json" ["Json", "Decode"]) Nothing

jsonDecodePipeline :: Import
jsonDecodePipeline = import_
  (ExternalModule "NoRedInk/elm-json-decode-pipeline"
                  ["Json", "Decode", "Pipeline"]
  )
  Nothing

jsonDecodeExtra :: Import
jsonDecodeExtra = import_
  (ExternalModule "elm-community/json-extra" ["Json", "Decode", "Extra"])
  Nothing

elmTime :: Import
elmTime = import_ (ExternalModule "elm/time" ["Time"]) Nothing

date :: Import
date = import_ (ExternalModule "justinmimbs/date" ["Date"]) Nothing

elmAccessors :: Import
elmAccessors =
  import_ (ExternalModule "bChiquet/elm-accessors" ["Accessors"]) Nothing
