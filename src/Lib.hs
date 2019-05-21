{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    )
where

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
import           Options.Applicative
import           Data.Semigroup                           ( (<>) )
import           Control.Monad                            ( join )

data Cmdline = Cmdline
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int
  }

run :: IO ()
run =
    join $ customExecParser (prefs showHelpOnEmpty) (info opts idm)

generate :: IO ()
generate = do
    dbURL      <- getEnv "DATABASE_URL"
    connection <- Connection.acquire (B.pack dbURL)
    case connection of
        Right connection' -> do
            let runner = runTable connection'
            runner "words"
            Connection.release connection'
        Left _ -> putStrLn "Couldn't establish connection to DB"

initConfig = return ()

opts :: Parser (IO ())
opts = subparser
    (  command
            "init"
            (info
                (pure initConfig)
                (progDesc "initialize the config file to run this program")
            )
    <> command "generate" (info (pure generate) (progDesc "generate elm files based on the config"))
    )

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
        Just r  -> putStrLn r
        Nothing -> putStrLn "Error"
