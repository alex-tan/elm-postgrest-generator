{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  )
where

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
import           System.Posix.Directory        as SPD
import qualified System.Directory              as D

data Command
  = Generate { table :: String , alias :: String , sourceDirectory :: String }
  | Commit String

run :: IO ()
run =
  let i :: ParserInfo Command
      i = info opts idm

      p :: ParserPrefs
      p = prefs showHelpOnEmpty
  in  do
        Generate table alias sourceDirectory <- customExecParser p i
        generate table alias sourceDirectory

generate :: String -> String -> String -> IO ()
generate table alias sourceDirectory = do
  dbURL      <- getEnv "DATABASE_URL"
  connection <- Connection.acquire (B.pack dbURL)
  case connection of
    Right connection' -> do
      let runner = runTable connection'
      runner table alias sourceDirectory
      Connection.release connection'
    Left _ -> putStrLn "Couldn't establish connection to DB"

opts :: Parser Command
opts = subparser generateCommand

-- command :: String -> ParserInfo a -> Mod CommandFields a
-- info :: Parser a -> InfoMod a -> ParserInfo a
-- strOption :: IsString s => Mod OptionFields s -> Parser s
-- strArgument :: IsString s => Mod ArgumentFields s -> Parser s

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
  )
  (progDesc "Generate elm files")

generators :: [(TableConfig -> [String], TableConfig -> Elm.ModuleFile)]
generators =
  [ (apiModule     , Generators.Api.generate)
  , (typesModule   , Generators.Types.generate)
  , (decodersModule, Generators.Decoders.generate)
  ]

runTable :: Connection.Connection -> String -> String -> String -> IO ()
runTable conn tableName alias sourceDirectory = do
  table' <- Session.run (tableSession tableName) conn

  case table' of
    Right table'' ->
      let config = TableConfig { specifiedTypeAlias       = Just alias
                               , specifiedModuleNamespace = Nothing
                               , C.table                  = table''
                               , C.sourceDirectory        = sourceDirectory
                               }
      in  mapM_ (runGenerator config) generators

    Left _ -> return ()

runGenerator
  :: TableConfig
  -> (TableConfig -> [String], TableConfig -> Elm.ModuleFile)
  -> IO ()
runGenerator config (toPath, generator) =
  let raw    = generator config
      result = toString raw
      src    = C.sourceDirectory config
  in  do
        exists <- D.doesDirectoryExist src
        unless exists $ D.createDirectory src
        case result of
          Just r ->
            let path     = intercalate "/" $ toPath config
                fullPath = (C.sourceDirectory config ++ path ++ ".elm")
            in  putStrLn ("Writing " ++ fullPath)
                  >> (createDirectoryRecursive src $ typesModule config)
                  >> writeFile fullPath r
          Nothing -> putStrLn "Error"

createDirectoryRecursive :: FilePath -> [FilePath] -> IO FilePath
createDirectoryRecursive base []       = return base
createDirectoryRecursive base (p : ps) = do
  let d = base </> p
  e <- D.doesDirectoryExist d
  unless e $ D.createDirectory d
  createDirectoryRecursive d ps
