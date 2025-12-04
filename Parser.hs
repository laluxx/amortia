{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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
import Control.Monad (void)
import GHC.Generics
import Data.Aeson (ToJSON)
import Prelude hiding (tail)

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
  | TTuple [Type]
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
  | PTuple [Pattern]
  | PLit Literal
  deriving (Show, Eq, Generic, ToJSON)

data Expr
  = EVar Text
  | ELit Literal
  | EApp Expr Expr
  | EError Text
  | EList [Expr]
  | ECons Expr Expr
  | ETuple [Expr]
  | ELambda [Text] Expr
  | ELet Text Expr Expr
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

symbolInline :: Text -> Parser Text
symbolInline = L.symbol scn

keywords :: [Text]
keywords = ["defn", "error", "display", "halt", "when"]

builtins :: [Text]
builtins = ["map", "car", "cdr"]

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

varNameInline :: Parser Text
varNameInline = try $ do
  name <- lexemeInline $ T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))
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
  , try typeTuple
  , typeParens
  , typeVarOrCon
  ] <?> "type"

typeUnit :: Parser Type
typeUnit = TUnit <$ symbol "()"

typeTuple :: Parser Type
typeTuple = between (symbol "{") (symbol "}") $ do
  types <- parseType `sepBy` symbol ","
  case types of
    [] -> fail "Empty tuple type"
    [_] -> fail "Single-element tuple type"
    _ -> pure $ TTuple types

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
typeList = between (symbol "[") (symbol "]") $ do
  first <- parseType
  option (TList first) $ do
    _ <- symbol ","
    rest <- parseType `sepBy` symbol ","
    return $ TList (TTuple (first : rest))

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
    go acc (TTuple ts) = foldl go acc ts
    go acc TUnit = acc

-- * Patterns

pattern' :: Parser Pattern
pattern' = choice
  [ try patternList
  , try patternTuple
  , PWild <$ symbol "_"
  , PLit <$> literal
  , PVar <$> varName
  ] <?> "pattern"

patternList :: Parser Pattern
patternList = between (symbol "[") (symbol "]") $ do
  option (PList []) $ do
    first <- pattern'
    choice
      [ do
          _ <- symbol "|"
          PCons first <$> pattern'
      , do
          rest <- many (symbol "," *> pattern')
          option (PList (first:rest)) $ do
            _ <- symbol "|"
            tail <- pattern'
            return $ foldr PCons tail (first:rest)
      ]

patternTuple :: Parser Pattern
patternTuple = between (symbol "{") (symbol "}") $ do
  pats <- pattern' `sepBy` symbol ","
  case pats of
    [] -> fail "Empty tuple pattern"
    [_] -> fail "Single-element tuple pattern"
    _ -> pure $ PTuple pats

-- * Expressions

expr :: Parser Expr
expr = makeExprParser exprApp operatorTable

exprApp :: Parser Expr
exprApp = do
  func <- exprAtom
  args <- many (try $ do
    pos1 <- getSourcePos
    hspace
    pos2 <- getSourcePos
    if sourceLine pos1 /= sourceLine pos2
      then fail "newline encountered"
      else exprAtom)
  pure $ foldl EApp func args

exprAtom :: Parser Expr
exprAtom = choice
  [ try exprLambda
  , try exprDisplay
  , try exprHalt
  , try exprError
  , try exprTuple
  , exprList
  , ELit <$> literal
  , between (symbol "(") (symbol ")") expr
  , try tightBinOp
  , EVar <$> varName
  ] <?> "expression"

exprLambda :: Parser Expr
exprLambda = do
  _ <- symbol "\\"
  params <- varName `sepBy1` hspace1
  _ <- symbol "->"
  body <- lambdaBody
  pure $ ELambda params body
  where
    -- Lambda body stops at comma (which delimits the lambda)
    lambdaBody = makeExprParser lambdaApp operatorTable
    
    lambdaApp = do
      func <- lambdaAtom
      args <- many (try $ do
        pos1 <- getSourcePos
        hspace
        pos2 <- getSourcePos
        if sourceLine pos1 /= sourceLine pos2
          then fail "newline encountered"
          else lookAhead (satisfy (/= ',')) >> lambdaAtom)
      pure $ foldl EApp func args
    
    lambdaAtom = choice
      [ try exprDisplay
      , try exprHalt
      , try exprError
      , try lambdaTuple
      , lambdaList
      , ELit <$> literal
      , between (symbol "(") (symbol ")") expr
      , try tightBinOp
      , EVar <$> varName
      ] <?> "expression"
    
    -- Lists in lambda body
    lambdaList = between (symbol "[") (symbol "]") $ do
      exprs <- lambdaListElem `sepBy` symbol ","
      option (EList exprs) $ do
        _ <- symbol "|"
        tailExpr <- lambdaListElem
        case exprs of
          [] -> fail "Cannot have empty list before | in expression"
          _ -> return $ foldr ECons tailExpr exprs
    
    lambdaListElem = makeExprParser lambdaListElemApp operatorTable
    
    lambdaListElemApp = do
      func <- lambdaAtom
      args <- many (try $ do
        pos1 <- getSourcePos
        hspace
        pos2 <- getSourcePos
        if sourceLine pos1 /= sourceLine pos2
          then fail "newline encountered"
          else do
            notFollowedBy (char ',')
            notFollowedBy (char ']')
            notFollowedBy (char '|')
            lambdaAtom)
      pure $ foldl EApp func args
    
    -- Tuples in lambda body
    lambdaTuple = between (symbol "{") (symbol "}") $ do
      exprs <- lambdaTupleElem `sepBy` symbol ","
      case exprs of
        [] -> fail "Empty tuple"
        [_] -> fail "Single-element tuple"
        _ -> pure $ ETuple exprs
    
    lambdaTupleElem = makeExprParser lambdaTupleElemApp operatorTable
    
    lambdaTupleElemApp = do
      func <- lambdaAtom
      args <- many (try $ do
        pos1 <- getSourcePos
        hspace
        pos2 <- getSourcePos
        if sourceLine pos1 /= sourceLine pos2
          then fail "newline encountered"
          else do
            notFollowedBy (char ',')
            notFollowedBy (char '}')
            lambdaAtom)
      pure $ foldl EApp func args

exprTuple :: Parser Expr
exprTuple = between (symbol "{") (symbol "}") $ do
  exprs <- expr `sepBy` symbol ","
  case exprs of
    [] -> fail "Empty tuple"
    [_] -> fail "Single-element tuple"
    _ -> pure $ ETuple exprs

tightBinOp :: Parser Expr
tightBinOp = try $ do
  e1 <- choice
    [ EVar <$> varNameNoSpace
    , ELit <$> literalNoSpace
    ]
  op <- choice
    [ "++" <$ try (string "++")
    , "*" <$ char '*'
    , "/" <$ char '/'
    , "+" <$ char '+'
    , "-" <$ try (char '-' <* notFollowedBy (char '>'))
    , ">=" <$ try (string ">=")
    , "=<" <$ try (string "<=")
    , "==" <$ try (string "==")
    , "/=" <$ try (string "/=")
    , ">" <$ try (char '>' <* notFollowedBy (char '='))
    , "<" <$ try (char '<' <* notFollowedBy (char '='))
    ]
  e2 <- choice
    [ try $ between (char '(') (char ')') expr
    , EVar <$> varNameNoSpace
    , ELit <$> literalNoSpace
    ]
  sc
  pure $ EBinOp op e1 e2

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (EBinOp "*" <$ symbol "*")
    , InfixL (EBinOp "/" <$ symbol "/")
    ]
  , [ InfixR (EBinOp "++" <$ try (symbol "++"))
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
exprList = between (symbol "[") (symbol "]") $ do
  exprs <- expr `sepBy` symbol ","
  option (EList exprs) $ do
    _ <- symbol "|"
    tailExpr <- expr
    case exprs of
      [] -> fail "Cannot have empty list before | in expression"
      _ -> return $ foldr ECons tailExpr exprs

-- * Statement-level expressions (for unit function bodies)

statement :: Parser Expr
statement = statementExpr

statementExpr :: Parser Expr
statementExpr = choice
  [ try statementDisplay
  , try statementHalt
  , try statementError
  , try statementLet
  , statementVarOrApp
  , statementList
  , ELit <$> literal
  ] <?> "statement"

statementLet :: Parser Expr
statementLet = do
  name <- varNameNoSpace
  hspace
  _ <- char '='
  hspace
  _ <- statementExpr
  return $ EVar name

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

symbolInlineNoArrow :: Text -> Parser Text
symbolInlineNoArrow s = try $ lexemeInline (string s <* notFollowedBy (string "->"))

exprInClause :: Parser Expr
exprInClause = letBinding <|> makeExprParser exprAppInClause operatorTableInClause
  where
    letBinding = try $ do
      name <- varNameInline
      _ <- symbolInline "="
      value <- makeExprParser exprAppInClause operatorTableInClause
      sc
      rest <- exprInClause
      return $ ELet name value rest
    
    exprAppInClause = do
      func <- exprAtomInClause
      args <- many (try $ do
        hspace
        notFollowedBy newline
        notFollowedBy (string "->")
        notFollowedBy (char '=')
        exprAtomInClause)
      let baseApp = foldl EApp func args
      -- Check for comma continuation (for lambda delimiter)
      option baseApp $ try $ do
        hspace
        _ <- char ','
        hspace
        rest <- exprAppInClause
        return $ EApp baseApp rest

    exprAtomInClause = choice
      [ try exprLambdaInClause
      , try exprDisplay
      , try exprHalt
      , try exprError
      , try exprTupleInClause
      , exprListInClause
      , ELit <$> literal
      , between (symbolInline "(") (symbolInline ")") exprInClause
      , try tightBinOpInClause
      , EVar <$> varNameInline
      ] <?> "expression"

    exprListInClause = between (symbolInline "[") (symbolInline "]") $ do
      exprs <- listElemInClause `sepBy` symbolInline ","
      option (EList exprs) $ do
        _ <- symbolInline "|"
        tailExpr <- listElemInClause
        case exprs of
          [] -> fail "Cannot have empty list before | in expression"
          _ -> return $ foldr ECons tailExpr exprs
    
    -- List elements should not continue application across commas
    listElemInClause = makeExprParser listElemAppInClause operatorTableInClause
    
    listElemAppInClause = do
      func <- exprAtomInClause
      args <- many (try $ do
        hspace
        notFollowedBy newline
        notFollowedBy (string "->")
        notFollowedBy (char '=')
        notFollowedBy (char ',')  -- Stop at comma in lists
        notFollowedBy (char ']')  -- Stop at list close
        notFollowedBy (char '|')  -- Stop at list tail
        exprAtomInClause)
      pure $ foldl EApp func args

    exprTupleInClause = between (symbolInline "{") (symbolInline "}") $ do
      exprs <- tupleElemInClause `sepBy` symbolInline ","
      case exprs of
        [] -> fail "Empty tuple"
        [_] -> fail "Single-element tuple"
        _ -> pure $ ETuple exprs
    
    -- Tuple elements should not continue application across commas
    tupleElemInClause = makeExprParser tupleElemAppInClause operatorTableInClause
    
    tupleElemAppInClause = do
      func <- exprAtomInClause
      args <- many (try $ do
        hspace
        notFollowedBy newline
        notFollowedBy (string "->")
        notFollowedBy (char '=')
        notFollowedBy (char ',')  -- Stop at comma in tuples
        notFollowedBy (char '}')  -- Stop at tuple close
        exprAtomInClause)
      pure $ foldl EApp func args

    tightBinOpInClause = try $ do
      e1 <- choice
        [ EVar <$> varNameNoSpace
        , ELit <$> literalNoSpace
        ]
      op <- choice
        [ "++" <$ try (string "++")
        , "*" <$ char '*'
        , "/" <$ char '/'
        , "+" <$ char '+'
        , "-" <$ try (char '-' <* notFollowedBy (char '>'))
        , ">=" <$ try (string ">=")
        , "=<" <$ try (string "<=")
        , "==" <$ try (string "==")
        , "/=" <$ try (string "/=")
        , ">" <$ try (char '>' <* notFollowedBy (oneOf ['=', '-']))
        , "<" <$ try (char '<' <* notFollowedBy (char '='))
        ]
      e2 <- choice
        [ try $ between (char '(') (char ')') exprInClause
        , EVar <$> varNameNoSpace
        , ELit <$> literalNoSpace
        ]
      hspace
      pure $ EBinOp op e1 e2

    operatorTableInClause =
      [ [ InfixL (EBinOp "*" <$ symbolInlineNoArrow "*")
        , InfixL (EBinOp "/" <$ symbolInlineNoArrow "/")
        ]
      , [ InfixR (EBinOp "++" <$ try (symbolInlineNoArrow "++"))
        , InfixL (EBinOp "+" <$ symbolInlineNoArrow "+")
        , InfixL (EBinOp "-" <$ try (symbolInlineNoArrow "-" <* notFollowedBy (char '>')))
        ]
      , [ InfixN (EBinOp ">" <$ try (symbolInlineNoArrow ">" <* notFollowedBy (oneOf ['=', '-'])))
        , InfixN (EBinOp "<" <$ symbolInlineNoArrow "<")
        , InfixN (EBinOp ">=" <$ symbolInlineNoArrow ">=")
        , InfixN (EBinOp "=<" <$ symbolInlineNoArrow "<=")
        , InfixN (EBinOp "==" <$ symbolInlineNoArrow "==")
        , InfixN (EBinOp "/=" <$ symbolInlineNoArrow "/=")
        ]
      ]

    exprLambdaInClause = do
      _ <- symbolInline "\\"
      params <- varName `sepBy1` hspace1
      _ <- symbolInline "->"
      body <- lambdaBodyInClause
      return $ ELambda params body
      where
        -- Lambda body in clause stops at comma
        lambdaBodyInClause = makeExprParser lambdaAppInClause operatorTableInClause
        
        lambdaAppInClause = do
          func <- lambdaAtomInClause
          args <- many (try $ do
            hspace
            notFollowedBy newline
            notFollowedBy (string "->")
            notFollowedBy (char '=')
            notFollowedBy (char ',')  -- Stop at comma
            lambdaAtomInClause)
          pure $ foldl EApp func args
        
        lambdaAtomInClause = choice
          [ try exprDisplay
          , try exprHalt
          , try exprError
          , try exprTupleInClause
          , exprListInClause
          , ELit <$> literal
          , between (symbolInline "(") (symbolInline ")") exprInClause
          , try tightBinOpInClause
          , EVar <$> varNameInline
          ] <?> "expression"

clause :: Parser Clause
clause = do
  choice
    [ try clauseWithNoPatterns
    , clauseWithPatterns
    ]
  where
    clauseWithNoPatterns = do
      hspace
      _ <- string "->"
      hspace
      Clause [] Nothing <$> exprInClause
    
    clauseWithPatterns = do
      hspace
      -- Try to parse as a let binding first (for single-line clauses)
      notFollowedBy (try $ varNameNoSpace >> hspace >> char '=')
      -- Parse patterns
      firstPat <- pattern'
      (morePats, grd) <- parsePatternsOnLine [firstPat]
      hspace
      _ <- string "->"
      hspace
      Clause (reverse morePats) grd <$> exprInClause
    
    parsePatternsOnLine acc = choice
      [ try $ do
          hspace
          _ <- char ','
          hspace
          _ <- string "when"
          hspace
          guardExpr <- manyTill anySingle (try $ lookAhead (hspace >> string "->"))
          case runParser (hspace *> exprInClause <* eof) "<guard>" (T.pack guardExpr) of
            Left err -> fail $ "Failed to parse guard: " ++ errorBundlePretty err
            Right g -> return (acc, Just (Guard g))
      , try $ do
          hspace
          notFollowedBy newline
          notFollowedBy (char ',')
          notFollowedBy (string "->")
          notFollowedBy $ try (varNameNoSpace >> hspace >> char '=')
          p <- pattern'
          parsePatternsOnLine (p:acc)
      , do
          return (acc, Nothing)
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
