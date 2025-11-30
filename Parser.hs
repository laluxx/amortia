{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Parser
  ( -- * Types
    Program(..)
  , TopLevel(..)
  , FunctionDefinition(..)
  , TypeSignature(..)
  , Type(..)
  , Clause(..)
  , Pattern(..)
  , Expr(..)
  , Literal(..)
    -- * Parser functions
  , parseProgram
  , parseFromFile
  , prettyError
    -- * Utility functions
  , hasMainFunction
    -- * Test functions
  , testExpr
  , testPattern
  , testClause
  , testFunctionDef
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import GHC.Generics
import Data.Aeson (ToJSON)

-- * Parser Type

type Parser = Parsec Void Text

-- * AST Definition

newtype Program = Program [TopLevel]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype TopLevel = FunctionDef FunctionDefinition
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data FunctionDefinition = FunctionDefinition
  { funName :: Text
  , funTypeSignature :: TypeSignature
  , funDocstring :: Maybe Text
  , funClauses :: [Clause]
  }
  deriving (Show, Eq, Generic, ToJSON)

data TypeSignature = TypeSignature
  { typeVars :: [Text]
  , typeExpr :: Type
  }
  deriving (Show, Eq, Generic, ToJSON)

data Type
  = TVar Text
  | TCon Text
  | TApp Type Type
  | TFun Type Type
  | TList Type
  | TUnit
  deriving (Show, Eq, Generic, ToJSON)

data Clause = Clause
  { clausePatterns :: [Pattern]
  , clauseBody :: Expr
  }
  deriving (Show, Eq, Generic, ToJSON)

data Pattern
  = PVar Text
  | PWild
  | PList [Pattern]
  | PCons Pattern Pattern
  | PLit Literal
  deriving (Show, Eq, Generic, ToJSON)

data Expr
  = EVar Text
  | ELit Literal
  | EApp Expr Expr
  | EError Text
  | EList [Expr]
  | EDisplay Expr
  | EHalt Expr
  | ESeq [Expr]
  deriving (Show, Eq, Generic, ToJSON)

data Literal
  = LInt Integer
  | LString Text
  | LAtom Text
  deriving (Show, Eq, Generic, ToJSON)

-- * Lexer

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

scn :: Parser ()
scn = L.space
  hspace1
  (L.skipLineComment "--")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeInline :: Parser a -> Parser a
lexemeInline = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol sc

keywords :: [Text]
keywords = ["defn", "error", "display", "halt"]

reserved :: Text -> Parser ()
reserved w = lexeme . try $ string w *> notFollowedBy alphaNumChar

-- * Identifiers

identifier :: Parser Text
identifier = (lexeme . fmap T.pack) p <?> "identifier"
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')

varName :: Parser Text
varName = try $ do
  name <- identifierInline
  if name `elem` keywords
    then fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    else pure name

identifierInline :: Parser Text
identifierInline = (lexemeInline . fmap T.pack) p <?> "identifier"
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')

-- * Literals

literal :: Parser Literal
literal = label "literal" $ choice
  [ LInt <$> integer
  , LString <$> stringLiteral
  ]

integer :: Parser Integer
integer = label "integer" $ lexeme L.decimal

stringLiteral :: Parser Text
stringLiteral = label "string literal" $ do
  char '"'
  content <- manyTill L.charLiteral (char '"')
  sc
  pure (T.pack content)

atom :: Parser Text
atom = label "atom" $ lexeme $ do
  c <- lowerChar
  cs <- many alphaNumChar
  pure (T.pack (c:cs))

-- * Type Expressions

parseType :: Parser Type
parseType = typeArrow
  where
    typeArrow = do
      t1 <- typeAtom
      option t1 $ do
        symbol "->"
        TFun t1 <$> parseType

typeAtom :: Parser Type
typeAtom = choice
  [ typeUnit
  , typeList
  , typeParens
  , typeVarOrCon
  ] <?> "type"

typeUnit :: Parser Type
typeUnit = TUnit <$ symbol "()"

typeVarOrCon :: Parser Type
typeVarOrCon = do
  name <- identifier
  if isTypeVar name
    then pure (TVar name)
    else do
      args <- many typeAtom
      pure $ foldl TApp (TCon name) args
  where
    isTypeVar name = case T.unpack name of
      (c:_) -> c `elem` ['a'..'z']
      _ -> False

typeList :: Parser Type
typeList = between (symbol "[") (symbol "]") $
  TList <$> parseType

typeParens :: Parser Type
typeParens = between (symbol "(") (symbol ")") parseType

typeSignature :: Parser (Text, TypeSignature)
typeSignature = do
  name <- varName
  symbol "::"
  ty <- parseType
  pure (name, TypeSignature (extractTypeVars ty) ty)

extractTypeVars :: Type -> [Text]
extractTypeVars = go []
  where
    go acc (TVar v) = if v `elem` acc then acc else v : acc
    go acc (TCon _) = acc
    go acc (TApp t1 t2) = go (go acc t1) t2
    go acc (TFun t1 t2) = go (go acc t1) t2
    go acc (TList t) = go acc t
    go acc TUnit = acc

-- * Patterns

pattern' :: Parser Pattern
pattern' = choice
  [ try patternCons
  , patternList
  , PWild <$ symbol "_"
  , PLit <$> literal
  , PVar <$> varName
  ] <?> "pattern"

patternList :: Parser Pattern
patternList = between (symbol "[") (symbol "]") $
  PList <$> pattern' `sepBy` symbol ","

patternCons :: Parser Pattern
patternCons = do
  symbol "["
  p <- pattern'
  symbol "|"
  ps <- pattern'
  symbol "]"
  pure $ PCons p ps

-- * Expressions

-- For unit function bodies: one statement per line
statement :: Parser Expr
statement = do
  -- Parse one line worth of expression
  e <- statementExpr
  -- Consume the newline or whitespace after it
  return e

statementExpr :: Parser Expr
statementExpr = choice
  [ try statementDisplay
  , try statementHalt
  , try statementError
  , statementVarOrApp
  , exprList
  , ELit <$> literal
  ] <?> "statement"

-- For regular clauses: completely unrestricted
expr :: Parser Expr
expr = exprAtom

exprAtom :: Parser Expr
exprAtom = choice
  [ try exprDisplay
  , try exprHalt
  , try exprError
  , exprList
  , ELit <$> literal
  , exprVarOrApp
  ] <?> "expression"

-- REGULAR function application (for clauses): unrestricted
exprVarOrApp :: Parser Expr
exprVarOrApp = do
  name <- varName
  args <- many exprAtom
  pure $ foldl EApp (EVar name) args

-- STATEMENT function application: consume args until newline
statementVarOrApp :: Parser Expr
statementVarOrApp = do
  name <- varNameNoSpace  -- Don't consume trailing space
  args <- many (try $ hspace1 *> statementArg)
  pure $ foldl EApp (EVar name) args

-- Variable name without consuming trailing space
varNameNoSpace :: Parser Text
varNameNoSpace = try $ do
  name <- T.pack <$> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` keywords
    then fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    else pure name

statementArg :: Parser Expr
statementArg = choice
  [ statementList  -- Use special list parser that doesn't consume newlines
  , ELit <$> literal
  , EVar <$> varName
  ]

-- List parser for statements (doesn't consume trailing newlines)
statementList :: Parser Expr
statementList = do
  char '['
  hspace
  exprs <- statementExpr `sepBy` (hspace *> char ',' <* hspace)
  hspace
  char ']'
  return (EList exprs)

exprError :: Parser Expr
exprError = do
  reserved "error"
  EError <$> atom

-- STATEMENT display: parse everything on the line
statementDisplay :: Parser Expr
statementDisplay = do
  string "display"
  notFollowedBy alphaNumChar
  hspace1
  -- Parse the argument which can be a function call
  arg <- statementDisplayArg
  return (EDisplay arg)

statementDisplayArg :: Parser Expr
statementDisplayArg = choice
  [ ELit <$> literal
  , statementList  -- Use statement list
  , statementVarOrApp  -- This handles "tail [1,2,3]"
  ]

-- REGULAR display: unrestricted
exprDisplay :: Parser Expr
exprDisplay = do
  reserved "display"
  EDisplay <$> exprAtom

exprHalt :: Parser Expr
exprHalt = do
  reserved "halt"
  EHalt <$> exprAtom

statementHalt :: Parser Expr
statementHalt = do
  string "halt"
  notFollowedBy alphaNumChar
  hspace1
  arg <- choice [ELit <$> literal, EVar <$> varName]
  return (EHalt arg)

statementError :: Parser Expr
statementError = do
  string "error"
  notFollowedBy alphaNumChar
  hspace1
  a <- atom
  return (EError a)

exprList :: Parser Expr
exprList = between (symbol "[") (symbol "]") $
  EList <$> expr `sepBy` symbol ","

-- * Clauses

clause :: Parser Clause
clause = do
  sc
  pats <- option [] $ try $ do
    ps <- pattern' `sepBy1` hspace1
    lookAhead (symbol "->")
    pure ps
  symbol "->"
  Clause pats <$> expr

-- * Docstrings

docstring :: Parser Text
docstring = stringLiteral <?> "docstring"

-- * Function Definitions

functionDef :: Parser FunctionDefinition
functionDef = do
  reserved "defn"
  (name, typeSig) <- typeSignature
  symbol "{"
  sc
  doc <- optional (try docstring <* sc)
  clauses <- if isUnitType (typeExpr typeSig)
    then unitFunctionBody
    else clause `sepEndBy` sc
  symbol "}"
  pure FunctionDefinition
    { funName = name
    , funTypeSignature = typeSig
    , funDocstring = doc
    , funClauses = clauses
    }

isUnitType :: Type -> Bool
isUnitType TUnit = True
isUnitType _ = False

-- THE FIX: Parse statements separated by whitespace
unitFunctionBody :: Parser [Clause]
unitFunctionBody = do
  exprs <- statement `sepEndBy` sc
  case exprs of
    [] -> pure []
    [e] -> pure [Clause [] e]
    es -> pure [Clause [] (ESeq es)]

-- * Top Level

topLevel :: Parser TopLevel
topLevel = (FunctionDef <$> functionDef) <?> "top-level definition"

program :: Parser Program
program = sc *> (Program <$> many topLevel) <* eof

-- * Main Parse Function

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = runParser program "<input>"

prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty

-- * Utility Functions

hasMainFunction :: Program -> Bool
hasMainFunction (Program topLevels) = any isMain topLevels
  where
    isMain (FunctionDef FunctionDefinition{funName = name}) = name == "main"

parseFromFile :: FilePath -> IO (Either String Program)
parseFromFile path = do
  input <- T.pack <$> readFile path
  pure $ case parseProgram input of
    Left err -> Left (prettyError err)
    Right ast -> Right ast

-- * Test individual parsers
testExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
testExpr = runParser expr "<test>"

testPattern :: Text -> Either (ParseErrorBundle Text Void) Pattern
testPattern = runParser pattern' "<test>"

testClause :: Text -> Either (ParseErrorBundle Text Void) Clause
testClause = runParser clause "<test>"

testFunctionDef :: Text -> Either (ParseErrorBundle Text Void) FunctionDefinition
testFunctionDef = runParser functionDef "<test>"
