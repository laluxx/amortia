{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
  T.unlines $ [moduleDecl, exportDecl, mainExp, ""] ++ 
              map generateTopLevel topLevels ++ 
              ["", mainFunction]
  where
    moduleDecl = "-module(" <> moduleName <> ")."
    exportDecl = "-export([" <> exports <> "])."
    mainExp = "-export([main/0])."
    exports = T.intercalate ", " $ map functionExport topLevels
    
    functionExport (FunctionDef FunctionDefinition{..}) =
      funName <> "/" <> T.pack (show $ length $ clausePatterns $ head funClauses)
    
    -- Generate main function
    mainFunction = T.unlines
      [ "main() ->"
      , "    io:format(\"Amortia program~n\"),"
      , "    halt(0)."
      ]

-- | Generate Erlang code from AST (legacy, uses default module name)
generateErlang :: Program -> Text
generateErlang prog = generateErlangWithModule "amortia" prog

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
    
    -- Generate clauses with proper semicolon/period handling
    clauseLines = case funClauses of
      [] -> []
      clauses -> zipWith (generateClause funName) clauses isLast
        where isLast = replicate (length clauses - 1) False ++ [True]

-- | Generate code for a single clause
generateClause :: Text -> Clause -> Bool -> Text
generateClause funcName Clause{..} isLast =
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
