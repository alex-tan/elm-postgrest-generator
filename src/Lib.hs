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
    , urlPrefix :: String
    }
  | Commit String

run :: IO ()
run =
  let i :: ParserInfo Command
      i = info opts idm

      p :: ParserPrefs
      p = prefs showHelpOnEmpty
  in  do
        Generate table alias sourceDirectory urlPrefix <- customExecParser p i
        generate table alias sourceDirectory urlPrefix

generate :: String -> String -> String -> String -> IO ()
generate table alias sourceDirectory urlPrefix = do
  dbURL      <- getEnv "DATABASE_URL"
  connection <- Connection.acquire (B.pack dbURL)
  case connection of
    Right connection' -> do
      let runner = runTable connection'
      runner table alias sourceDirectory urlPrefix
      Connection.release connection'
    Left _ -> putStrLn "Couldn't establish connection to DB"

opts :: Parser Command
opts = subparser generateCommand

generateCommand :: Mod CommandFields Command
generateCommand = command "generate" $ info
  (   Generate
  <$> strArgument (metavar "table" <> help "the name of the table")
  <*> strOption (long "alias" <> help "The elm type alias; e.g. for a `directors` table the type alias might be Director")
  <*> strOption
        (  long "output"
        <> help "The root directory to output the elm code"
        <> value "./src"
        <> showDefault
        )
  <*> strOption
        (  long "url-prefix"
        <> help "URL to prefix your postgrest endpoints with. e.g. /api"
        <> value "/"
        <> showDefault
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
runTable conn tableName alias sourceDirectory urlPrefix = do
  table' <- Session.run (tableSession tableName) conn

  case table' of
    Right table'' ->
      let config = TableConfig { specifiedTypeAlias       = Just alias
                               , specifiedModuleNamespace = Nothing
                               , C.table                  = table''
                               , C.sourceDirectory        = sourceDirectory
                               , C.urlPrefix              = urlPrefix
                               }
      in  mapM_ (runGenerator config) generators

    Left _ -> return ()

runGenerator :: TableConfig -> (TableConfig -> Elm.ModuleFile) -> IO ()
runGenerator config generator =
  let moduleFile   = generator config
      fileContents = toString moduleFile
      src          = C.sourceDirectory config
  in  do
        exists <- D.doesDirectoryExist src
        unless exists $ D.createDirectory src
        case fileContents of
          Just r ->
            let pathParts = moduleNameParts moduleFile
                path      = intercalate "/" pathParts
                fullPath  = C.sourceDirectory config ++ path ++ ".elm"
            in  putStrLn ("Writing " ++ fullPath)
                  >> ( createDirectoryRecursive src
                     . map fromString
                     . take (length pathParts - 1)
                     $ pathParts
                     )
                  >> writeFile fullPath r
          Nothing -> putStrLn "Error generating elm file contents."

createDirectoryRecursive :: FilePath -> [FilePath] -> IO FilePath
createDirectoryRecursive base []       = return base
createDirectoryRecursive base (p : ps) = do
  let directory = base </> p
  exists <- D.doesDirectoryExist directory
  unless exists $ D.createDirectory directory
  createDirectoryRecursive directory ps
