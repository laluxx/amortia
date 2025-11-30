{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Codegen
  ( generateErlang
  , generateErlangFile
  , generateErlangWithModule
  ) where

import Parser
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import System.Process (readProcessWithExitCode, callCommand)
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName)


-- | Generate Erlang code from AST with module name
generateErlangWithModule :: Text -> Program -> Text
generateErlangWithModule moduleName (Program topLevels) =
  T.unlines $ [moduleDecl, exportDecl] ++
              map generateTopLevel topLevels
  where
    moduleDecl = "-module(" <> moduleName <> ")."
    exportDecl = "-export([" <> exports <> "])."
    exports = T.intercalate ", " $ map functionExport topLevels

    functionExport (FunctionDef FunctionDefinition{..}) =
      let arity = if null funClauses
                  then 0
                  else length $ clausePatterns $ head funClauses
      in funName <> "/" <> T.pack (show arity)

-- | Generate Erlang code from AST (legacy, uses default module name)
generateErlang :: Program -> Text
generateErlang = generateErlangWithModule "amortia"

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
      Just doc -> map ("%~ " <>) (T.lines doc)

    -- Generate clauses with proper semicolon/period handling
    -- Handle empty function case
    clauseLines = case funClauses of
      [] -> [funName <> "() -> ok."]
      clauses -> zipWith (generateClause funName) clauses isLast
        where isLast = replicate (length clauses - 1) False ++ [True]

-- | Generate code for a single clause
generateClause :: Text -> Clause -> Bool -> Text
generateClause funcName Clause{..} isLast =
  case clauseBody of
    ESeq exprs ->
      -- For sequences, put arrow on first line, then indent expressions
      funcName <> "(" <> patterns <> ") ->\n    " <>
      T.intercalate ",\n    " (map generateExpr exprs) <>
      terminator
    _ ->
      -- For single expressions, keep on one line
      funcName <> "(" <> patterns <> ") -> " <> body <> terminator
  where
    patterns = T.intercalate ", " $ map generatePattern clausePatterns
    body = generateExpr clauseBody
    terminator = if isLast then "." else ";"

-- | Generate code for a pattern
generatePattern :: Pattern -> Text
generatePattern pat = case pat of
  PVar name -> capitalize name
  PWild -> "_"
  PList pats -> "[" <> T.intercalate ", " (map generatePattern pats) <> "]"
  PCons p ps -> "[" <> generatePattern p <> "|" <> generatePattern ps <> "]"
  PLit lit -> generateLiteral lit

-- | Generate code for an expression
generateExpr :: Expr -> Text
generateExpr expr = case expr of
  EVar name -> capitalize name  -- Variables are uppercase
  ELit lit -> generateLiteral lit
  EApp e1 e2 -> generateApp e1 e2
  EError atom -> "error(" <> atom <> ")"
  EList exprs -> "[" <> T.intercalate ", " (map generateExpr exprs) <> "]"
  EDisplay e -> generateDisplay e
  EHalt e -> "halt(" <> generateExpr e <> ")"
  ESeq exprs -> T.intercalate ", " (map generateExpr exprs)

-- | Generate display expression with smart formatting
generateDisplay :: Expr -> Text
generateDisplay expr = 
  case expr of
    -- If it's a string literal, use it as the format string
    ELit (LString s) -> "io:format(\"" <> s <> "\")"
    -- Otherwise, auto-format with ~p~n
    _ -> "io:format(\"~p~n\", [" <> generateExpr expr <> "])"

-- | Generate a function application
-- The leftmost element is the function name (keep lowercase)
-- All arguments are expressions (variables get uppercased)
generateApp :: Expr -> Expr -> Text
generateApp func arg =
  let (fname, args) = collectArgs func [arg]
  in case fname of
       EVar name -> name <> "(" <> T.intercalate ", " (map generateExpr args) <> ")"
       _ -> generateExpr func <> "(" <> generateExpr arg <> ")"
  where
    -- Collect all arguments from nested EApp
    collectArgs (EApp f a) acc = collectArgs f (a:acc)
    collectArgs f acc = (f, acc)

-- | Convert variable name to uppercase (Erlang convention)
capitalize :: Text -> Text
capitalize = T.toUpper

-- | Generate code for a literal
generateLiteral :: Literal -> Text
generateLiteral lit = case lit of
  LInt n -> T.pack (show n)
  LString s -> s  -- Don't add quotes here since we add them where needed
  LAtom a -> a

-- | Generate Erlang code and compile to BEAM with runner script
generateErlangFile :: FilePath -> Program -> IO ()
generateErlangFile outputFile program = do
  let moduleName = T.pack $ takeBaseName outputFile
  let erlangCode = generateErlangWithModule moduleName program
  writeFile outputFile (T.unpack erlangCode)
  compileErlang outputFile
  -- Create a runner script
  createRunnerScript moduleName outputFile

-- | Compile Erlang file to BEAM bytecode
compileErlang :: FilePath -> IO ()
compileErlang erlFile = do
  result <- readProcessWithExitCode "erlc" [erlFile] ""
  case result of
    (ExitSuccess, _, _) -> return ()
    (ExitFailure _, _, err) -> putStrLn $ "Erlang compilation failed:\n" ++ err

-- | Create a runner script for the BEAM module
createRunnerScript :: Text -> FilePath -> IO ()
createRunnerScript moduleName erlFile = do
  let scriptName = takeBaseName erlFile  -- e.g., "src"
  let scriptContent = unlines
        [ "#!/bin/bash"
        , "erl -noshell -pa . -s " ++ T.unpack moduleName ++ " main -s init stop"
        ]
  writeFile scriptName scriptContent
  callCommand $ "chmod +x " ++ scriptName
