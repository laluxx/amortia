{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen
  ( generateErlang
  , generateErlangFile
  ) where

import Parser
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)


-- | Generate Erlang code from AST
generateErlang :: Program -> Text
generateErlang (Program topLevels) =
  T.unlines $ map generateTopLevel topLevels

-- | Generate code for a top-level definition
generateTopLevel :: TopLevel -> Text
generateTopLevel (FunctionDef funcDef) = generateFunction funcDef

-- | Generate code for a function definition
generateFunction :: FunctionDefinition -> Text
generateFunction FunctionDefinition{..} =
  T.unlines $ filter (not . T.null) parts
  where
    parts = docstringLines ++ clauseLines
    
    docstringLines = case funDocstring of
      Nothing -> []
      Just doc -> map (\line -> "%~ " <> line) (T.lines doc)
    
    clauseLines = map (generateClause funName) funClauses

-- | Generate code for a single clause
generateClause :: Text -> Clause -> Text
generateClause funcName Clause{..} =
  funcName <> "(" <> patterns <> ") -> " <> body <> ";"
  where
    patterns = T.intercalate ", " $ map generatePattern clausePatterns
    body = generateExpr clauseBody

-- | Generate code for a pattern
generatePattern :: Pattern -> Text
generatePattern pat = case pat of
  PVar name -> capitalize name
  PWild -> "_"
  PList pats -> "[" <> T.intercalate ", " (map generatePattern pats) <> "]"
  PCons p ps -> "[" <> generatePattern p <> "|" <> generatePattern ps <> "]"
  PLit lit -> generateLiteral lit

-- | Generate code for an expression
-- In expressions, we need to distinguish between:
-- - Variables (which should be uppercase)
-- - Function calls (which should stay lowercase)
generateExpr :: Expr -> Text
generateExpr expr = case expr of
  EVar name -> capitalize name  -- Variables are uppercase
  ELit lit -> generateLiteral lit
  EApp e1 e2 -> generateApp e1 e2
  EError atom -> "error(" <> atom <> ")"
  EList exprs -> "[" <> T.intercalate ", " (map generateExpr exprs) <> "]"

-- | Generate a function application
-- The leftmost element is the function name (keep lowercase)
-- All arguments are expressions (variables get uppercased)
generateApp :: Expr -> Expr -> Text
generateApp func arg =
  case func of
    -- Base case: simple function call
    EVar fname -> fname <> "(" <> generateExpr arg <> ")"
    -- Nested application: func is already being applied to something
    EApp f a -> generateApp f a <> ", " <> generateExpr arg
    -- Other cases (shouldn't happen in well-formed code)
    _ -> generateExpr func <> "(" <> generateExpr arg <> ")"

-- | Convert variable name to uppercase (Erlang convention)
capitalize :: Text -> Text
capitalize = T.toUpper

-- | Generate code for a literal
generateLiteral :: Literal -> Text
generateLiteral lit = case lit of
  LInt n -> T.pack (show n)
  LString s -> "\"" <> s <> "\""
  LAtom a -> a

-- | Generate Erlang code and write to file
generateErlangFile :: FilePath -> Program -> IO ()
generateErlangFile outputFile program = do
  let erlangCode = generateErlang program
  writeFile outputFile (T.unpack erlangCode)
