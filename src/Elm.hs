{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

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

import qualified Data.Text                     as Text
import qualified ElmFormat.Parse               as Parse
import           Prelude                           hiding ( exp )
import qualified Data.Map.Strict               as Map
import           Data.List                                ( intercalate
                                                          , intersperse
                                                          )
import qualified Data.List.Index               as DI
import           Data.Maybe                               ( catMaybes )
import qualified Data.Set                      as Set
import           Generation
import           Data.Char                     as Char
import qualified Reporting.Result as RR
import qualified ElmFormat.Render.Text as Render
import ElmVersion

data ModuleFile = ModuleFile
  { moduleNameParts :: [String]
  , expressions :: [Expression]
  } deriving (Show)

toString :: ModuleFile -> Maybe String
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
      contents =
        intercalate "\n"
        . intercalate [blankLine]
        . filter (not . null)
        $ [ [moduleLine]
          , map importLine $ Map.elems $ imports importsAndExports_
          , (map (expressionToString 0) . expressions) moduleFile
          ]
  in
    fmap (Text.unpack . Render.render Elm_0_19) . RR.toMaybe . Parse.parse . Text.pack $ contents

pipeRightChain :: Expression -> [Expression] -> Expression
pipeRightChain start parts = applyEach . intersperse LineEnd $ (start : parts)

applyEach :: [Expression] -> Expression
applyEach (x : []) = x
applyEach (x : xs) = Call x [applyEach xs]
applyEach []       = LocalReference ""

type IndentLevel = Int

expressionToString :: IndentLevel -> Expression -> String
expressionToString iL expr =
  let
    toStringAtCurrentIndent = expressionToString iL

    nextIndentLevel         = iL + 1
                          -- nextIndent = expressionToString (iL + 1)
  in
    case expr of
      TypeVariable s -> s
      Int_         i -> show i
      Call e es ->
        let firstString = expressionToString iL e
            call'       = unwords (firstString : map toStringAtCurrentIndent es)
        in  if isOperator firstString
              then indent iL call'
              else parenthesize call'
      FunctionDeclaration _ type' name exp -> intercalate
        "\n"
        (sig : assignment : toStringAtCurrentIndent exp : [blankLine])
       where
        sig        = name ++ " : " ++ toStringAtCurrentIndent type'
        assignment = name ++ " ="
      ExternalReference q i f -> importFunction q i f
      Lambda params expr' ->
        parenthesize
          $  "\\"
          ++ unwords params
          ++ " -> "
          ++ toStringAtCurrentIndent expr'
      LineEnd          -> "\n"
      LocalReference f -> f
      Parentheses es ->
        parenthesize . unwords . map toStringAtCurrentIndent $ es
      Record _ -> 
        "TODO"
      RecordDeclaration _ name fields ->
        intercalate ""
          . intersperse "\n"
          . concat
          $ [ [name ++ " :"]
            , DI.imap recordDeclarationSignatureToString fields
            , [indent nextIndentLevel "}"]
            , [name ++ " ="]
            , DI.imap recordDeclarationFieldToString fields
            , [indent nextIndentLevel "}", blankLine]
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
            , intercalate " -> "
              . map toStringAtCurrentIndent $rdfSignature rdf
            ]
          )
          where opener = if index == 0 then "{" else ","
        recordDeclarationFieldToString
          :: Int -> RecordDeclarationField -> String
        recordDeclarationFieldToString index rdf = indent
          1
          (unwords
            [opener, rdfName rdf, "=", toStringAtCurrentIndent $ rdfValue rdf]
          )
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
          nextIndentLevel
          (opener ++ " " ++ toStringAtCurrentIndent expr')
          where opener = if index == 0 then "{" else ","
      UpdateRecord recordVar fields ->
        unwords
          $  ["{", recordVar, "|"]
          ++ map recordFieldUpdateToString fields
          ++ ["}"]

      Field name exps ->
        unwords $ name : ":" : map toStringAtCurrentIndent exps

recordFieldUpdateToString :: (FieldName, Expression) -> [Char]
recordFieldUpdateToString (name, expr) =
  unwords [name, "=", expressionToString 0 expr]

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
  Lambda _ expr       -> importsAndExports [expr]
  LineEnd             -> emptyIE
  LocalReference _    -> emptyIE
  Parentheses    exps -> importsAndExports exps
  Record fields ->
    importsAndExports . map snd $ fields
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
  | Field FieldName [Expression]
  | FunctionDeclaration Public Expression FunctionName Expression
  | Int_ Prelude.Int
  | Lambda [LambdaParameter] Expression
  | LineEnd
  | LocalReference FunctionName
  | Parentheses [Expression]
  | Str String
  | TypeAlias Public TypeAliasName [Expression]
  | Record [RecordField]
  | RecordDeclaration Public RecordName [RecordDeclarationField]
  | TypeVariable String
  | UpdateRecord RecordVariable [(FieldName, Expression)]
  deriving (Show)

data RecordDeclarationField = RecordDeclarationField
  { rdfName :: String
  , rdfSignature :: [Expression]
  , rdfValue :: Expression
  } deriving (Show)

type RecordField = (String, Expression)

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
