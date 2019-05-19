{-# LANGUAGE NamedFieldPuns #-}

module Elm
  ( Expression(..)
  , Exposure(..)
  , Public(..)
  , Import
  , ModuleFile(..)
  , Qualification(..)
  , RecordDeclarationField(..)
  , Module(..)
  , exposeSome
  , import_
  , importAs
  , toString
  , qualifiedReference
  , unqualifiedReference
  , exposedFunction
  , pipeRightChain
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

pipeRightChain :: Expression -> [Expression] -> Expression
pipeRightChain start parts = applyEach $ (start : parts)

applyEach :: [Expression] -> Expression
applyEach (x : []) = x
applyEach (x : xs) = Call x [applyEach xs]
applyEach []       = LocalReference ""

expressionToString :: Expression -> String
expressionToString expr = case expr of
  TypeVariable s -> s
  Int_         i -> show i
  Call e es ->
    let firstString = expressionToString e
        call'       = unwords (firstString : map expressionToString es)
    in  if isOperator firstString
          then "\n" ++ indent 1 call'
          else parenthesize call'
  FunctionDeclaration _ type' name exp -> intercalate
    "\n"
    (sig : assignment : (indent 1 . expressionToString) exp : [blankLine])
   where
    sig        = name ++ " : " ++ expressionToString type'
    assignment = name ++ " ="
  ExternalReference q i f -> importFunction q i f
  Lambda params expr' ->
    parenthesize $ "\\" ++ unwords params ++ " -> " ++ expressionToString expr'
  LocalReference f  -> f
  Parentheses    es -> parenthesize . unwords . map expressionToString $ es
  RecordDeclaration _ name fields ->
    intercalate "\n"
      . concat
      $ [ [name ++ " :"]
        , DI.imap recordDeclarationSignatureToString fields
        , [indent 1 "}"]
        , [name ++ " ="]
        , DI.imap recordDeclarationFieldToString fields
        , [indent 1 "}", blankLine]
        ]
   where
    recordDeclarationSignatureToString
      :: Int -> RecordDeclarationField -> String
    recordDeclarationSignatureToString index rdf = indent
      1
      (unwords
        [ opener
        , rdfName rdf
        , ":"
        , intercalate " -> " . map expressionToString $rdfSignature rdf
        ]
      )
      where opener = if index == 0 then "{" else ","
    recordDeclarationFieldToString :: Int -> RecordDeclarationField -> String
    recordDeclarationFieldToString index rdf = indent
      1
      (unwords [opener, rdfName rdf, "=", expressionToString $ rdfValue rdf])
      where opener = if index == 0 then "{" else ","
  Str s                   -> "\"" ++ s ++ "\""
  TypeAlias _ name fields -> intercalate
    "\n"
    (  ("type alias " ++ name ++ " =")
    :  DI.imap fieldToString fields
    ++ [indent 1 "}", blankLine]
    )
   where
    fieldToString :: Int -> Expression -> String
    fieldToString index expr' = indent
      1
      (opener ++ " " ++ expressionToString expr')
      where opener = if index == 0 then "{" else ","
  UpdateRecord recordVar fields ->
    unwords
      $  ["{", recordVar, "|"]
      ++ map recordFieldUpdateToString fields
      ++ ["}"]

  Field name exps -> unwords $ name : ":" : map expressionToString exps

recordFieldUpdateToString :: (FieldName, Expression) -> [Char]
recordFieldUpdateToString (name, expr) =
  unwords [name, "=", expressionToString expr]

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

addExport :: String -> ImportsAndExports -> ImportsAndExports
addExport s i = i { exports = Set.insert s (exports i) }

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
  Call exp' exps ->
    mergeIE (importsAndExportsFromExpression exp') (importsAndExports exps)
  Field _ exps                  -> importsAndExports exps
  Int_         _                -> emptyIE
  TypeVariable _                -> emptyIE
  ExternalReference q import' f -> emptyIE
    { imports =
      Map.fromList
        [(moduleName . module_ $ import', setQualification q f import')]
    }
  FunctionDeclaration Public exp name exp' ->
    foldIE (emptyIE { exports = Set.fromList [name] }) (mapExps [exp, exp'])
  FunctionDeclaration Private exp _ exp' -> importsAndExports [exp, exp']
  Lambda _ expr                          -> importsAndExports [expr]
  LocalReference _                       -> emptyIE
  Parentheses    exps                    -> importsAndExports exps
  RecordDeclaration Public r fields ->
    addExport r
      . importsAndExports
      . concatMap (\f -> rdfValue f : rdfSignature f)
      $ fields
  RecordDeclaration Private _ fields ->
    importsAndExports . concatMap (\f -> rdfValue f : rdfSignature f) $ fields
  Str _ -> emptyIE
  TypeAlias Public name exps ->
    foldIE (emptyIE { exports = Set.fromList [name] }) (mapExps exps)
  TypeAlias Private _ exps -> importsAndExports exps
  UpdateRecord _ fields    -> importsAndExports . map snd $ fields
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
type RecordName = String
type TypeAliasName = String
type LambdaParameter = String
type RecordVariable = String

data Public
  = Public
  | Private
  deriving (Show)

data Expression
  = Call Expression [Expression]
  | ExternalReference Qualification Import FunctionName
  | FunctionDeclaration Public Expression FunctionName Expression
  | LocalReference FunctionName
  | Parentheses [Expression]
  | Str String
  | TypeAlias Public TypeAliasName [Expression]
  | Field FieldName [Expression]
  | Int_ Prelude.Int
  | RecordDeclaration Public RecordName [RecordDeclarationField]
  | TypeVariable String
  | Lambda [LambdaParameter] Expression
  | UpdateRecord RecordVariable [(FieldName, Expression)]
  deriving (Show)

data RecordDeclarationField = RecordDeclarationField
  { rdfName :: String
  , rdfSignature :: [Expression]
  , rdfValue :: Expression
  } deriving (Show)

exposedFunction :: Expression -> FunctionName -> Expression -> Expression
exposedFunction = FunctionDeclaration Public

qualifiedReference :: Import -> FunctionName -> Expression
qualifiedReference = ExternalReference Qualified

unqualifiedReference :: Import -> FunctionName -> Expression
unqualifiedReference = ExternalReference Unqualified

data Qualification
  = Qualified
  | Unqualified
  deriving (Show)
