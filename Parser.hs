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
  , Guard(..)
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
  , clauseGuard :: Maybe Guard
  , clauseBody :: Expr
  }
  deriving (Show, Eq, Generic, ToJSON)

data Guard = Guard Expr
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
  | EBinOp Text Expr Expr
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
keywords = ["defn", "error", "display", "halt", "when"]

reserved :: Text -> Parser ()
reserved w = lexeme . try $ string w *> notFollowedBy alphaNumChar

-- * Identifiers

identifier :: Parser Text
identifier = (lexeme . fmap T.pack) p <?> "identifier"
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

varName :: Parser Text
varName = try $ do
  name <- identifier
  if name `elem` keywords
    then fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    else pure name

varNameNoSpace :: Parser Text
varNameNoSpace = try $ do
  name <- T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
  if name `elem` keywords
    then fail $ "keyword " ++ T.unpack name ++ " cannot be used as identifier"
    else pure name

-- * Literals

literal :: Parser Literal
literal = label "literal" $ choice
  [ LInt <$> integer
  , LString <$> stringLiteral
  ]

integer :: Parser Integer
integer = label "integer" $ lexeme L.decimal

literalNoSpace :: Parser Literal
literalNoSpace = choice
  [ LInt <$> (read <$> some digitChar)
  ]

stringLiteral :: Parser Text
stringLiteral = label "string literal" $ do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"')
  sc
  pure (T.pack content)

atom :: Parser Text
atom = label "atom" $ lexeme $ do
  c <- lowerChar
  cs <- many alphaNumChar
  pure (T.pack (c:cs))

-- Atom that only consumes horizontal space, not newlines
atomNoNewline :: Parser Text
atomNoNewline = label "atom" $ lexemeInline $ do
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
        _ <- symbol "->"
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
  _ <- symbol "::"
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
  _ <- symbol "["
  p <- pattern'
  _ <- symbol "|"
  ps <- pattern'
  _ <- symbol "]"
  pure $ PCons p ps

-- * Expressions

-- Top-level expression parser with operators
expr :: Parser Expr
expr = makeExprParser exprApp operatorTable

-- Function application (binds tighter than operators)
-- Only consumes arguments until a newline is encountered
exprApp :: Parser Expr
exprApp = do
  func <- exprAtom
  args <- many (try $ hspace >> notFollowedBy newline >> exprAtom)
  pure $ foldl EApp func args

-- Atomic expressions
exprAtom :: Parser Expr
exprAtom = choice
  [ try exprDisplay
  , try exprHalt
  , try exprError
  , exprList
  , ELit <$> literal
  , between (symbol "(") (symbol ")") expr  -- Parenthesized expressions
  , try tightBinOp  -- Binary ops without spaces (n-1)
  , EVar <$> varName
  ] <?> "expression"

-- Parse binary operations WITHOUT spaces (like n-1, x+1, etc.)
tightBinOp :: Parser Expr
tightBinOp = try $ do
  -- Parse first operand (variable or literal)
  e1 <- choice
    [ EVar <$> varNameNoSpace
    , ELit <$> literalNoSpace
    ]
  -- NO space before operator
  op <- choice
    [ "++" <$ try (string "++")
    , "*" <$ char '*'
    , "/" <$ char '/'
    , "+" <$ char '+'
    , "-" <$ (char '-' <* notFollowedBy (char '>'))  -- Make sure - is not followed by >
    , ">=" <$ try (string ">=")
    , "=<" <$ try (string "<=")
    , "==" <$ try (string "==")
    , "/=" <$ try (string "/=")
    , ">" <$ char '>'
    , "<" <$ char '<'
    ]
  -- NO space after operator
  e2 <- choice
    [ try $ between (char '(') (char ')') expr  -- Allow (expr) as second operand
    , EVar <$> varNameNoSpace
    , ELit <$> literalNoSpace
    ]
  -- Consume trailing whitespace
  sc
  pure $ EBinOp op e1 e2

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (EBinOp "*" <$ symbol "*")
    , InfixL (EBinOp "/" <$ symbol "/")
    ]
  , [ InfixR (EBinOp "++" <$ try (symbol "++"))  -- Must come BEFORE "+"
    , InfixL (EBinOp "+" <$ symbol "+")
    , InfixL (EBinOp "-" <$ symbol "-")
    ]
  , [ InfixN (EBinOp ">" <$ symbol ">")
    , InfixN (EBinOp "<" <$ symbol "<")
    , InfixN (EBinOp ">=" <$ symbol ">=")
    , InfixN (EBinOp "=<" <$ symbol "<=")
    , InfixN (EBinOp "==" <$ symbol "==")
    , InfixN (EBinOp "/=" <$ symbol "/=")
    ]
  ]

exprError :: Parser Expr
exprError = do
  reserved "error"
  EError <$> atomNoNewline

exprDisplay :: Parser Expr
exprDisplay = do
  reserved "display"
  EDisplay <$> exprAtom

exprHalt :: Parser Expr
exprHalt = do
  reserved "halt"
  EHalt <$> exprAtom

exprList :: Parser Expr
exprList = between (symbol "[") (symbol "]") $
  EList <$> expr `sepBy` symbol ","

-- * Statement-level expressions (for unit function bodies)

statement :: Parser Expr
statement = statementExpr

statementExpr :: Parser Expr
statementExpr = choice
  [ try statementDisplay
  , try statementHalt
  , try statementError
  , statementVarOrApp
  , statementList
  , ELit <$> literal
  ] <?> "statement"

statementVarOrApp :: Parser Expr
statementVarOrApp = do
  name <- varNameNoSpace
  args <- many (try $ hspace1 *> statementArg)
  pure $ foldl EApp (EVar name) args

statementArg :: Parser Expr
statementArg = choice
  [ statementList
  , ELit <$> literal
  , EVar <$> varName
  ]

statementList :: Parser Expr
statementList = do
  _ <- char '['
  hspace
  exprs <- statementExpr `sepBy` (hspace *> char ',' <* hspace)
  hspace
  _ <- char ']'
  return (EList exprs)

statementDisplay :: Parser Expr
statementDisplay = do
  _ <- string "display"
  notFollowedBy alphaNumChar
  hspace1
  EDisplay <$> statementDisplayArg

statementDisplayArg :: Parser Expr
statementDisplayArg = choice
  [ ELit <$> literal
  , statementList
  , statementVarOrApp
  ]

statementHalt :: Parser Expr
statementHalt = do
  _ <- string "halt"
  notFollowedBy alphaNumChar
  hspace1
  arg <- choice [ELit <$> literal, EVar <$> varName]
  return (EHalt arg)

statementError :: Parser Expr
statementError = do
  _ <- string "error"
  notFollowedBy alphaNumChar
  hspace1
  EError <$> atom

-- * Clauses

clause :: Parser Clause
clause = do
  sc
  choice
    [ try $ do
        -- No patterns case (for unit functions)
        _ <- symbol "->"
        Clause [] Nothing <$> expr
    , do
        -- Has patterns case
        firstPat <- pattern'
        let continuePatterns acc = do
              hspace
              choice
                [ try $ do
                    -- Guard coming: ", when EXPR ->"
                    _ <- string ", when"
                    hspace
                    -- Parse guard expression but stop at "->"
                    guardExpr <- manyTill anySingle (try $ lookAhead (hspace >> string "->"))
                    case runParser (sc *> expr <* sc <* eof) "<guard>" (T.pack guardExpr) of
                      Left err -> fail $ "Failed to parse guard: " ++ errorBundlePretty err
                      Right g -> return (reverse acc, Just (Guard g))
                , try $ do
                    -- Arrow coming, no more patterns
                    _ <- lookAhead (string "->")
                    return (reverse acc, Nothing)
                , do
                    -- Another pattern coming
                    p <- pattern'
                    continuePatterns (p:acc)
                ]
        (pats, grd) <- continuePatterns [firstPat]
        -- Parse arrow and body
        hspace
        _ <- symbol "->"
        Clause pats grd <$> expr
    ]

-- * Docstrings

docstring :: Parser Text
docstring = stringLiteral <?> "docstring"

-- * Function Definitions

functionDef :: Parser FunctionDefinition
functionDef = do
  reserved "defn"
  (name, typeSig) <- typeSignature
  _ <- symbol "{"
  sc
  doc <- optional (try docstring <* sc)
  clauses <- if isUnitType (typeExpr typeSig)
    then unitFunctionBody
    else clause `sepEndBy` sc
  _ <- symbol "}"
  pure FunctionDefinition
    { funName = name
    , funTypeSignature = typeSig
    , funDocstring = doc
    , funClauses = clauses
    }

isUnitType :: Type -> Bool
isUnitType TUnit = True
isUnitType _ = False

unitFunctionBody :: Parser [Clause]
unitFunctionBody = do
  exprs <- statement `sepEndBy` sc
  case exprs of
    [] -> pure []
    [e] -> pure [Clause [] Nothing e]
    es -> pure [Clause [] Nothing (ESeq es)]

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
