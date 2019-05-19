{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    )
where

import           Prelude
import qualified Hasql.Session                 as Session
import qualified Hasql.Connection              as Connection
import           TableDefinition                          ( tableSession )
import qualified Generators.Api
import qualified Generators.Types
import qualified Generators.Decoders
import           Elm                                      ( toString )
import           Config
import           System.Environment                       ( getEnv )
import qualified Data.ByteString.Char8         as B

run :: IO ()
run = do
    dbURL            <- getEnv "DATABASE_URL"
    Right connection <- Connection.acquire (B.pack dbURL)
    let tableName = "words"
    table' <- Session.run (tableSession tableName) connection
    case table' of
        Right table'' -> do
            let config = TableConfig { specifiedTypeAlias       = Just "Word"
                                     , specifiedModuleNamespace = Nothing
                                     , table                    = table''
                                     }
            let a = toString $ Generators.Types.generate config
            let b = toString $ Generators.Decoders.generate config
            let c = toString $ Generators.Api.generate config

            putStrLn a
            putStrLn b
            putStrLn c
            -- writeFile "output/Types.elm"    a
            -- writeFile "output/Decoders.elm" b
            -- writeFile "output/Api.elm"      b

        Left _ -> return ()
