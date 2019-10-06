{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  )
where

import           Data.String                              ( fromString )
import           System.FilePath.Posix                    ( (</>) )
import           Data.List                                ( intercalate )
import qualified Hasql.Session                 as Session
import qualified Hasql.Connection              as Connection
import           TableDefinition                          ( tableSession )
import qualified Generators.Api
import qualified Generators.Types
import qualified Generators.Decoders
import           Elm                                      ( toString
                                                          , ModuleFile
                                                            ( moduleNameParts
                                                            )
                                                          )
import           Config                        as C
import           System.Environment                       ( getEnv )
import qualified Data.ByteString.Char8         as B
import           Text.Pretty.Simple
import           Options.Applicative
import           Data.Semigroup                           ( (<>) )
import           Control.Monad                            ( join
                                                          , when
                                                          , unless
                                                          )
import qualified System.Directory              as D

data Command
  = Generate
    { table :: String
    , alias :: String
    , sourceDirectory :: String
    , apiPrefix :: String
    }
  | Commit String

run :: IO ()
run =
  let i :: ParserInfo Command
      i = info opts idm

      p :: ParserPrefs
      p = prefs showHelpOnEmpty
  in  do
        Generate table alias sourceDirectory apiPrefix <- customExecParser p i
        generate table alias sourceDirectory apiPrefix

generate :: String -> String -> String -> String -> IO ()
generate table alias sourceDirectory apiPrefix = do
  dbURL      <- getEnv "DATABASE_URL"
  connection <- Connection.acquire (B.pack dbURL)
  case connection of
    Right connection' -> do
      let runner = runTable connection'
      runner table alias sourceDirectory apiPrefix
      Connection.release connection'
    Left _ -> putStrLn "Couldn't establish connection to DB"

opts :: Parser Command
opts = subparser generateCommand

generateCommand :: Mod CommandFields Command
generateCommand = command "generate" $ info
  (   Generate
  <$> strArgument (metavar "table" <> help "the name of the table")
  <*> strOption (long "alias" <> help "The elm type alias")
  <*> strOption
        (  long "output"
        <> help "The root directory to output the elm code"
        <> value "./src"
        )
  <*> strOption
        (  long "api-prefix"
        <> help "URL to prefix your postgrest endpoints with."
        <> value "/"
        )
  )
  (progDesc "Generate elm files")

generators :: [TableConfig -> Elm.ModuleFile]
generators =
  [ Generators.Api.generate
  , Generators.Types.generate
  , Generators.Decoders.generate
  ]

runTable
  :: Connection.Connection -> String -> String -> String -> String -> IO ()
runTable conn tableName alias sourceDirectory apiPrefix = do
  table' <- Session.run (tableSession tableName) conn

  case table' of
    Right table'' ->
      let config = TableConfig { specifiedTypeAlias       = Just alias
                               , specifiedModuleNamespace = Nothing
                               , C.table                  = table''
                               , C.sourceDirectory        = sourceDirectory
                               , C.apiPrefix              = apiPrefix
                               }
      in  mapM_ (runGenerator config) generators

    Left _ -> return ()

runGenerator :: TableConfig -> (TableConfig -> Elm.ModuleFile) -> IO ()
runGenerator config generator =
  let raw    = generator config
      result = toString raw
      src    = C.sourceDirectory config
  in  do
        exists <- D.doesDirectoryExist src
        unless exists $ D.createDirectory src
        case result of
          Just r ->
            let parts    = moduleNameParts raw
                path     = intercalate "/" parts
                fullPath = (C.sourceDirectory config ++ path ++ ".elm")
            in  putStrLn ("Writing " ++ fullPath)
                  >> ( createDirectoryRecursive src
                     . map fromString
                     . take ((length parts) - 1)
                     $ parts
                     )
                  >> writeFile fullPath r
          Nothing -> putStrLn "Error"

createDirectoryRecursive :: FilePath -> [FilePath] -> IO FilePath
createDirectoryRecursive base []       = return base
createDirectoryRecursive base (p : ps) = do
  let d = base </> p
  e <- D.doesDirectoryExist d
  unless e $ D.createDirectory d
  createDirectoryRecursive d ps
