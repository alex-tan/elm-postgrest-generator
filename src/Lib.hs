{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  )
where

import           Data.List                                ( intercalate )
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

data Command
  = Generate String String
  | Commit String

run :: IO ()
run =
  let i :: ParserInfo Command
      i = info opts idm

      p :: ParserPrefs
      p = prefs showHelpOnEmpty
  in  do
        Generate table alias <- customExecParser p i
        generate table alias

generate :: String -> String -> IO ()
generate table alias = do
  dbURL      <- getEnv "DATABASE_URL"
  connection <- Connection.acquire (B.pack dbURL)
  case connection of
    Right connection' -> do
      let runner = runTable connection'
      runner table alias
      Connection.release connection'
    Left _ -> putStrLn "Couldn't establish connection to DB"

opts :: Parser Command
opts = subparser (generateCommand <> commitCommand)

-- command :: String -> ParserInfo a -> Mod CommandFields a
-- info :: Parser a -> InfoMod a -> ParserInfo a
-- strOption :: IsString s => Mod OptionFields s -> Parser s
-- strArgument :: IsString s => Mod ArgumentFields s -> Parser s

generateCommand :: Mod CommandFields Command
generateCommand = command "generate" $ info
  (Generate <$> strArgument (help "the name of the table") <*> strOption
    (long "alias" <> help "The elm type alias")
  )
  (progDesc "Generate elm files")

commitCommand :: Mod CommandFields Command
commitCommand = command "commit" $ info
  (Commit <$> strOption
    (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
  )
  (progDesc "Record changes to the repository")

generators :: [TableConfig -> Elm.ModuleFile]
generators =
  [ Generators.Api.generate
  , Generators.Types.generate
  , Generators.Decoders.generate
  ]

runTable :: Connection.Connection -> String -> String -> IO ()
runTable conn tableName alias = do
  table' <- Session.run (tableSession tableName) conn

  case table' of
    Right table'' ->
      let config = TableConfig
            { specifiedTypeAlias       = Just alias
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
    Just r ->
      let path = intercalate "/" $ typesModule config
      in  writeFile ("./elm/" ++ path) r
    Nothing -> putStrLn "Error"
