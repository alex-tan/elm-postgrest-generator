{-# LANGUAGE NamedFieldPuns #-}

module Elm
  ( Expression(..)
  , Exposure(..)
  , Public(..)
  , Import
  , ModuleFile(..)
  , Qualification(..)
  , Module(..)
  , exposeSome
  , import_
  , importAs
  , toString
  , qualifiedReference
  , unqualifiedReference
  , exposedFunction
  )
where

import           Prelude                           hiding ( exp )
import qualified Data.Map.Strict               as Map
import           Data.List                                ( intercalate )
import qualified Data.List.Index               as DI
import           Data.Maybe                               ( catMaybes )
import qualified Data.Set                      as Set
import           Generation
import           Data.Char                     as Char

data ModuleFile = ModuleFile
  { moduleNameParts :: [String]
  , expressions :: [Expression]
  } deriving (Show)

toString :: ModuleFile -> String
toString moduleFile =
  let moduleName' :: String
      moduleName'        = intercalate "." (moduleNameParts moduleFile)
      importsAndExports_ = importsAndExports $ expressions moduleFile
      moduleLine         = unwords
        [ "module"
        , moduleName'
        , "exposing"
        , parenthesize
        . intercalate ", "
        . Set.toList
        . exports
        $ importsAndExports_
        ]
  in  intercalate "\n"
        . intercalate [blankLine]
        . filter (not . null)
        $ [ [moduleLine]
          , map importLine $ Map.elems $ imports importsAndExports_
          , (map expressionToString . expressions) moduleFile
          ]

expressionToString :: Expression -> String
expressionToString expr = case expr of
  Int_ i -> show i
  Call e es ->
    let firstString = expressionToString e
        call'       = unwords (firstString : map expressionToString es)
    in  if isOperator firstString then call' else parenthesize call'
  FunctionDeclaration _ type' name exps -> intercalate
    "\n"
    (sig : assignment : map (indent 1 . expressionToString) exps ++ [blankLine])
   where
    sig        = name ++ " : " ++ expressionToString type'
    assignment = name ++ " ="
  ExternalReference q i f -> importFunction q i f
  LocalReference f        -> f
  Parentheses es -> parenthesize . unwords . map expressionToString $ es
  Str            s        -> "\"" ++ s ++ "\""
  TypeAlias _ name fields -> intercalate
    "\n"
    (  ("type alias " ++ name ++ " =")
    :  DI.imap fieldToString fields
    ++ [indent 1 "}"]
    )

   where
    fieldToString :: Int -> Expression -> String
    fieldToString index expr' = indent
      1
      (opener ++ " " ++ expressionToString expr')
      where opener = if index == 0 then "{" else ","
  Field name exps -> unwords $ name : ":" : map expressionToString exps

isOperator :: String -> Bool
isOperator (x : _) = not $ Char.isAlphaNum x
isOperator _       = False

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

importLine :: Import -> String
importLine import' =
  unwords
    .  catMaybes
    $  [Just "import", (Just . moduleName . module_) import']
    ++ qualification
    ++ exposure'
 where
  qualification = case alias import' of
    Just name -> [Just "as", Just name]
    Nothing   -> []
  exposure' = case exposing import' of
    ExposeNone -> []
    ExposeSome a ->
      [Just "exposing", Just . parenthesize . intercalate ", " . Set.toList $ a]
    ExposeAll -> [Just "exposing", Just "(..)"]

data ImportsAndExports = ImportsAndExports
  { imports :: Map.Map String Import
  , exports :: Set.Set String
  }

emptyIE :: ImportsAndExports
emptyIE = ImportsAndExports { imports = Map.empty, exports = Set.empty }

mergeIE :: ImportsAndExports -> ImportsAndExports -> ImportsAndExports
mergeIE a b = ImportsAndExports
  { exports = Set.union (exports a) (exports b)
  , imports = Map.unionWith mergeSameImportExposures (imports a) (imports b)
  }

importsAndExports :: [Expression] -> ImportsAndExports
importsAndExports = foldIE emptyIE . map importsAndExportsFromExpression

foldIE :: ImportsAndExports -> [ImportsAndExports] -> ImportsAndExports
foldIE = foldl mergeIE

importsAndExportsFromExpression :: Expression -> ImportsAndExports
importsAndExportsFromExpression expression = case expression of
  Int_ _ -> emptyIE
  Call exp' exps ->
    mergeIE (importsAndExportsFromExpression exp') (importsAndExports exps)
  ExternalReference q import' f -> emptyIE
    { imports =
      Map.fromList
        [(moduleName . module_ $ import', setQualification q f import')]
    }
  FunctionDeclaration Public exp name exps ->
    foldIE (emptyIE { exports = Set.fromList [name] }) (mapExps $ exp : exps)
  FunctionDeclaration Private exp _ exps -> importsAndExports (exp : exps)
  LocalReference _                       -> emptyIE
  Parentheses    exps                    -> importsAndExports exps
  Str            _                       -> emptyIE
  TypeAlias Public name exps ->
    foldIE (emptyIE { exports = Set.fromList [name] }) (mapExps exps)
  TypeAlias Private _ exps -> importsAndExports exps
  Field _ exps             -> importsAndExports exps
  where mapExps = map importsAndExportsFromExpression

data Import
  = Import
    { module_ :: Module
    , alias :: Maybe String
    , exposing :: Exposure
    } deriving (Show, Ord, Eq)

setQualification :: Qualification -> String -> Import -> Import
setQualification qual func imp@Import { exposing } = case qual of
  Qualified   -> imp
  Unqualified -> imp
    { exposing = case exposing of
                   ExposeAll           -> exposing
                   ExposeNone          -> ExposeSome $ Set.fromList [func]
                   ExposeSome existing -> ExposeSome $ Set.insert func existing
    }

-- Assumes the module is the same between both
mergeSameImportExposures :: Import -> Import -> Import
mergeSameImportExposures a b = case (exposing a, exposing b) of
  (ExposeNone, _         ) -> b
  (_         , ExposeNone) -> a
  (ExposeAll , _         ) -> a
  (_         , ExposeAll ) -> b
  (ExposeSome someA, ExposeSome someB) ->
    a { exposing = ExposeSome $ Set.union someA someB }

importFunction :: Qualification -> Import -> String -> String
importFunction q i f = case q of
  Qualified   -> explicitFunction i f
  Unqualified -> f

explicitFunction :: Import -> String -> String
explicitFunction i f = case alias i of
  Just a  -> dotJoin a f
  Nothing -> dotJoin (moduleName (module_ i)) f

dotJoin :: String -> String -> String
dotJoin a b = a ++ "." ++ b

import_ :: Module -> Maybe String -> Import
import_ a b = Import a b ExposeNone

data Module
  = ExternalModule Package ModuleName
  | LocalModule ModuleName
  deriving (Show, Ord, Eq)

moduleName :: Module -> String
moduleName (ExternalModule _ a) = a
moduleName (LocalModule a     ) = a

importAs :: String -> String -> Import
importAs name alias = Import { alias    = Just alias
                             , module_  = LocalModule name
                             , exposing = ExposeNone
                             }

data Exposure
  = ExposeAll
  | ExposeSome (Set.Set String)
  | ExposeNone
  deriving (Show, Ord, Eq)

exposeSome :: [String] -> Exposure
exposeSome = ExposeSome . Set.fromList

type FieldName = String
type FunctionName = String
type ModuleName = String
type Package = String
type TypeAliasName = String

data Public
  = Public
  | Private
  deriving (Show)

data Expression
  = Call Expression [Expression]
  | ExternalReference Qualification Import FunctionName
  | FunctionDeclaration Public Expression FunctionName [Expression]
  | LocalReference FunctionName
  | Parentheses [Expression]
  | Str String
  | TypeAlias Public TypeAliasName [Expression]
  | Field FieldName [Expression]
  | Int_ Prelude.Int
  deriving (Show)

exposedFunction :: Expression -> FunctionName -> [Expression] -> Expression
exposedFunction = FunctionDeclaration Public

qualifiedReference :: Import -> FunctionName -> Expression
qualifiedReference = ExternalReference Qualified

unqualifiedReference :: Import -> FunctionName -> Expression
unqualifiedReference = ExternalReference Unqualified

data Qualification
  = Qualified
  | Unqualified
  deriving (Show)
