{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TableDefinition
    ( tableSession
    , Table(..)
    , Column(..)
    , elmDataTypeToString
    , elmDataTypeToExpression
    , ElmDataType(..)
    )
where

import           Prelude
import           Hasql.Session                            ( Session )
import           Hasql.Statement                          ( Statement(..) )
import qualified Hasql.Session                 as Session
import qualified Hasql.Decoders                as HD
import qualified Hasql.Encoders                as HE
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSU
import qualified Data.Map.Strict               as Map
import           Data.Maybe                               ( fromMaybe )
import           Elm

tableSession :: String -> Session Table
tableSession table = do
  -- Get the sum of a and b
    cols <- Session.statement () (tableStatement table)
    return $ Table table cols

data Table = Table
    { name :: String
    , columns :: [Column]
    } deriving (Show)

data Column = Column
    { columnName :: String
    , dataType :: ElmDataType
    , isNullable :: Bool
    } deriving (Show)

data ElmDataType
    = String
    | Int
    | Float
    | Bool
    | Unknown String
    | Posix
    | Date
    deriving (Show)

elmDataTypeToString :: ElmDataType -> String
elmDataTypeToString (Unknown s) = s
elmDataTypeToString s           = show s

elmDataTypeToExpression :: ElmDataType -> Expression
elmDataTypeToExpression e = case e of
    Posix -> unqualifiedReference elmTime "Posix"
    Date  -> unqualifiedReference elmDate "Date"
    any'  -> local $ elmDataTypeToString any'

elmTime :: Import
elmTime = import_ (ExternalModule "elm/time" "Time") Nothing

elmDate :: Import
elmDate = import_ (ExternalModule "justinmimbs/date" "Date") Nothing

tableStatement :: String -> Statement () [Column]
tableStatement table = Statement sql HE.unit decoder True  where
    sql :: BS.ByteString
    sql =
        BSU.pack
            $ "select column_name, data_type, is_nullable = 'YES' as is_nullable from INFORMATION_SCHEMA.COLUMNS where table_name = '"
            ++ table
            ++ "'"
    decoder = HD.rowList tblRow
    tblRow =
        columnConstructor
            <$> HD.column HD.bytea
            <*> HD.column HD.bytea
            <*> HD.column HD.bool

-- https://www.postgresql.org/docs/9.5/datatype.html
postgresToElmTypes :: Map.Map BS.ByteString ElmDataType
postgresToElmTypes = Map.fromList
        -- Numeric
    [ ("smallint"        , Int)
    , ("integer"         , Int)
    , ("bigint"          , Int)
    , ("decimal"         , Float)
    , ("numeric"         , Float)
    , ("real"            , Float)
    , ("double precision", Float)
    , ("smallserial"     , Int)
    , ("serial"          , Int)
    , ( "bigserial"
      , Int
      )
        -- Monetary
    , ( "money"
      , Float
      )
        -- Character Types
    , ("character varying", String)
    , ("varchar"          , String)
    , ("character"        , String)
    , ("char"             , String)
    , ( "text"
      , String
      )
        -- Time
    , ("timestamp with time zone", Posix)
    , ("timestamp"               , Posix)
    , ( "date"
      , Date
      )
        -- Boolean
    , ("boolean", Bool)
    ]

toDataType :: BS.ByteString -> ElmDataType
toDataType bs =
    fromMaybe (Unknown $ BSU.unpack bs) $ Map.lookup bs postgresToElmTypes

columnConstructor :: BS.ByteString -> BS.ByteString -> Bool -> Column
columnConstructor columnName dataType =
    Column (BSU.unpack columnName) (toDataType dataType)


