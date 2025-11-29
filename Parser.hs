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
  = TVar Text              -- Type variable: a, b, c
  | TCon Text              -- Type constructor: Int, String
  | TApp Type Type         -- Type application: Maybe a
  | TFun Type Type         -- Function type: a -> b
  | TList Type             -- List type: [a]
  deriving (Show, Eq, Generic, ToJSON)

data Clause = Clause
  { clausePatterns :: [Pattern]
  , clauseBody :: Expr
  }
  deriving (Show, Eq, Generic, ToJSON)

data Pattern
  = PVar Text              -- Variable pattern: x
  | PWild                  -- Wildcard: _
  | PList [Pattern]        -- List pattern: [x, y]
  | PCons Pattern Pattern  -- Cons pattern: [x|xs]
  | PLit Literal           -- Literal pattern: 42, "hello"
  deriving (Show, Eq, Generic, ToJSON)

data Expr
  = EVar Text              -- Variable: x
  | ELit Literal           -- Literal: 42, "hello"
  | EApp Expr Expr         -- Application: f x
  | EError Text            -- Error: error badarg
  | EList [Expr]           -- List: [1, 2, 3]
  deriving (Show, Eq, Generic, ToJSON)

data Literal
  = LInt Integer
  | LString Text
  | LAtom Text
  deriving (Show, Eq, Generic, ToJSON)

-- * Lexer

-- | Space consumer that handles whitespace and comments
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

-- | Space consumer that only handles horizontal space (for inline tokens)
scn :: Parser ()
scn = L.space
  hspace1
  (L.skipLineComment "--")
  empty

-- | Lexeme parser wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Inline lexeme (doesn't consume newlines)
lexemeInline :: Parser a -> Parser a
lexemeInline = L.lexeme scn

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Reserved words
keywords :: [Text]
keywords = ["defn", "error"]

reserved :: Text -> Parser ()
reserved w = lexeme . try $ string w *> notFollowedBy alphaNumChar

-- * Identifiers

-- | Parse a lower-case identifier
identifier :: Parser Text
identifier = (lexeme . fmap T.pack) p <?> "identifier"
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')

-- | Parse a variable name (identifier that's not a keyword)
varName :: Parser Text
varName = try $ do
  name <- identifierInline
  if name `elem` keywords
    then fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    else pure name

-- | Parse a lower-case identifier (inline version)
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

-- | Parse a type expression with proper precedence
parseType :: Parser Type
parseType = typeArrow
  where
    typeArrow = do
      t1 <- typeAtom
      option t1 $ do
        symbol "->"
        TFun t1 <$> parseType

-- | Parse atomic type expressions
typeAtom :: Parser Type
typeAtom = choice
  [ typeList
  , typeParens
  , typeVarOrCon
  ] <?> "type"

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

-- * Type Signatures

typeSignature :: Parser (Text, TypeSignature)
typeSignature = do
  name <- varName
  symbol "::"
  ty <- parseType
  pure (name, TypeSignature (extractTypeVars ty) ty)

-- | Extract all type variables from a type
extractTypeVars :: Type -> [Text]
extractTypeVars = go []
  where
    go acc (TVar v) = if v `elem` acc then acc else v : acc
    go acc (TCon _) = acc
    go acc (TApp t1 t2) = go (go acc t1) t2
    go acc (TFun t1 t2) = go (go acc t1) t2
    go acc (TList t) = go acc t

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

expr :: Parser Expr
expr = exprAtom

exprAtom :: Parser Expr
exprAtom = choice
  [ exprError
  , exprList
  , ELit <$> literal
  , exprVarOrApp
  ] <?> "expression"

exprVarOrApp :: Parser Expr
exprVarOrApp = do
  name <- varName
  args <- many exprAtom
  pure $ foldl EApp (EVar name) args

exprError :: Parser Expr
exprError = reserved "error" *> (EError <$> varName)

exprList :: Parser Expr
exprList = between (symbol "[") (symbol "]") $
  EList <$> expr `sepBy` symbol ","

-- * Clauses

clause :: Parser Clause
clause = do
  sc  -- consume leading whitespace including indentation
  pats <- pattern' `sepBy1` hspace1
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
  clauses <- clause `sepEndBy` sc
  symbol "}"
  pure FunctionDefinition
    { funName = name
    , funTypeSignature = typeSig
    , funDocstring = doc
    , funClauses = clauses
    }

-- * Top Level

topLevel :: Parser TopLevel
topLevel = (FunctionDef <$> functionDef) <?> "top-level definition"

program :: Parser Program
program = sc *> (Program <$> many topLevel) <* eof

-- * Main Parse Function

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = runParser program "<input>"

-- | Format parse errors nicely
prettyError :: ParseErrorBundle Text Void -> String
prettyError = errorBundlePretty

-- * Utility Functions

-- | Run parser and return result with pretty errors
parseFromFile :: FilePath -> IO (Either String Program)
parseFromFile path = do
  input <- T.pack <$> readFile path
  pure $ case parseProgram input of
    Left err -> Left (prettyError err)
    Right ast -> Right ast

-- | Test individual parsers
testExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
testExpr = runParser expr "<test>"

testPattern :: Text -> Either (ParseErrorBundle Text Void) Pattern
testPattern = runParser pattern' "<test>"

testClause :: Text -> Either (ParseErrorBundle Text Void) Clause
testClause = runParser clause "<test>"

testFunctionDef :: Text -> Either (ParseErrorBundle Text Void) FunctionDefinition
testFunctionDef = runParser functionDef "<test>"
