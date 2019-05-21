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
import           Elm                                      ( toString
                                                          , ModuleFile
                                                          )
import           Config
import           System.Environment                       ( getEnv )
import qualified Data.ByteString.Char8         as B
import           Text.Pretty.Simple

run :: IO ()
run = do
    dbURL      <- getEnv "DATABASE_URL"
    connection <- Connection.acquire (B.pack dbURL)
    case connection of
        Right connection' -> do

            let runner = runTable connection'
            runner "words"
            Connection.release connection'
        Left _ -> putStrLn "Couldn't establish connection to DB"

generators :: [TableConfig -> Elm.ModuleFile]
generators =
    [ Generators.Types.generate
    , Generators.Decoders.generate
    , Generators.Api.generate
    ]

runTable :: Connection.Connection -> String -> IO ()
runTable conn tableName = do
    table' <- Session.run (tableSession tableName) conn

    case table' of
        Right table'' ->
            let config = TableConfig { specifiedTypeAlias       = Just "Word"
                                     , specifiedModuleNamespace = Nothing
                                     , table                    = table''
                                     }
            in  mapM_ (runGenerator config) generators

        Left _ -> return ()

runGenerator :: TableConfig -> (TableConfig -> Elm.ModuleFile) -> IO ()
runGenerator config generator = do
    let raw    = generator config
    let result = toString raw
    case result of
        Just r -> do
            putStrLn r
        Nothing -> putStrLn "Error"
        -- pPrint raw
