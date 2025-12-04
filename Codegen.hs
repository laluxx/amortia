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

    clauseLines = case funClauses of
      [] -> [funName <> "() -> ok."]
      clauses -> zipWith (generateClause funName) clauses isLast
        where isLast = replicate (length clauses - 1) False ++ [True]

-- | Generate code for a single clause
generateClause :: Text -> Clause -> Bool -> Text
generateClause funcName Clause{..} isLast =
  case clauseBody of
    ESeq exprs ->
      funcName <> "(" <> patterns <> ")" <> guardText <> " ->\n    " <>
      T.intercalate ",\n    " (map generateExpr exprs) <>
      terminator
    _ ->
      funcName <> "(" <> patterns <> ")" <> guardText <> " -> " <> body <> terminator
  where
    patterns = T.intercalate ", " $ map generatePattern clausePatterns
    guardText = case clauseGuard of
      Nothing -> ""
      Just g -> " " <> generateGuard g
    body = generateExpr clauseBody
    terminator = if isLast then "." else ";"

-- | Generate code for a guard
generateGuard :: Guard -> Text
generateGuard (Guard expr) = "when " <> generateExpr expr

-- | Generate code for a pattern
generatePattern :: Pattern -> Text
generatePattern pat = case pat of
  PVar name -> capitalize name
  PWild -> "_"
  PList pats -> "[" <> T.intercalate ", " (map generatePattern pats) <> "]"
  PCons p ps -> generateConsPat p ps
  PTuple pats -> "{" <> T.intercalate ", " (map generatePattern pats) <> "}"
  PLit lit -> generateLiteral lit
  where
    generateConsPat :: Pattern -> Pattern -> Text
    generateConsPat p rest = 
      let (heads, maybeTail) = collectConsHeads rest [p]
      in case maybeTail of
           Just tailPat -> "[" <> T.intercalate ", " (map generatePattern heads) <> "|" <> generatePattern tailPat <> "]"
           Nothing -> "[" <> T.intercalate ", " (map generatePattern heads) <> "]"
    
    collectConsHeads :: Pattern -> [Pattern] -> ([Pattern], Maybe Pattern)
    collectConsHeads (PCons p rest) acc = collectConsHeads rest (acc ++ [p])
    collectConsHeads (PList []) acc = (acc, Nothing)
    collectConsHeads other acc = (acc, Just other)

-- | Generate code for an expression
generateExpr :: Expr -> Text
generateExpr expr = case expr of
  EVar name -> 
    -- Special handling for built-in keywords
    case name of
      "map" -> "lists:map"
      "car" -> "hd"
      "cdr" -> "tl"
      _ -> capitalize name
  ELit lit -> generateLiteral lit
  EApp e1 e2 -> generateApp e1 e2
  EError atom -> "error(" <> atom <> ")"
  EList exprs -> "[" <> T.intercalate ", " (map generateExpr exprs) <> "]"
  ECons e1 e2 -> generateConsExpr e1 e2
  ETuple exprs -> "{" <> T.intercalate ", " (map generateExpr exprs) <> "}"
  ELambda params body -> generateLambda params body
  ELet name value rest -> generateLet name value rest
  EDisplay e -> generateDisplay e
  EHalt e -> "halt(" <> generateExpr e <> ")"
  ESeq exprs -> T.intercalate ", " (map generateExpr exprs)
  EBinOp op e1 e2 -> generateBinOp op e1 e2
  where
    generateConsExpr :: Expr -> Expr -> Text
    generateConsExpr e rest = 
      let (heads, maybeTail) = collectConsExprs rest [e]
      in case maybeTail of
           Just tailExpr -> "[" <> T.intercalate ", " (map generateExpr heads) <> "|" <> generateExpr tailExpr <> "]"
           Nothing -> "[" <> T.intercalate ", " (map generateExpr heads) <> "]"
    
    collectConsExprs :: Expr -> [Expr] -> ([Expr], Maybe Expr)
    collectConsExprs (ECons e rest) acc = collectConsExprs rest (acc ++ [e])
    collectConsExprs (EList []) acc = (acc, Nothing)
    collectConsExprs other acc = (acc, Just other)

-- | Generate lambda expression
generateLambda :: [Text] -> Expr -> Text
generateLambda params body =
  "fun(" <> T.intercalate ", " (map capitalize params) <> ") -> " <>
  generateExpr body <> " end"

-- | Generate let binding
generateLet :: Text -> Expr -> Expr -> Text
generateLet name value rest =
  capitalize name <> " = " <> generateExpr value <> ", " <> generateExpr rest

-- | Generate code for an expression in argument position (may need parens)
generateExprArg :: Expr -> Text
generateExprArg expr = case expr of
  EBinOp op e1 e2 -> "(" <> generateBinOp op e1 e2 <> ")"
  _ -> generateExpr expr

-- | Generate binary operator expression
generateBinOp :: Text -> Expr -> Expr -> Text
generateBinOp op e1 e2 =
  let erlangOp = case op of
        "<=" -> "=<"
        "/=" -> "/="
        _ -> op
  in generateExpr e1 <> " " <> erlangOp <> " " <> generateExpr e2

-- | Generate display expression with smart formatting
generateDisplay :: Expr -> Text
generateDisplay expr = 
  case expr of
    ELit (LString s) -> "io:format(\"" <> s <> "\")"
    _ -> "io:format(\"~p~n\", [" <> generateExpr expr <> "])"

-- | Generate a function application
generateApp :: Expr -> Expr -> Text
generateApp func arg =
  let (fname, args) = collectArgs func [arg]
  in case fname of
       EVar name -> case name of
         "map" -> "lists:map(" <> T.intercalate ", " (map generateExprArg args) <> ")"
         "car" -> "hd(" <> T.intercalate ", " (map generateExprArg args) <> ")"
         "cdr" -> "tl(" <> T.intercalate ", " (map generateExprArg args) <> ")"
         _ -> name <> "(" <> T.intercalate ", " (map generateExprArg args) <> ")"
       _ -> generateExpr func <> "(" <> generateExprArg arg <> ")"
  where
    collectArgs (EApp f a) acc = collectArgs f (a:acc)
    collectArgs f acc = (f, acc)

-- | Convert variable name to uppercase (Erlang convention)
capitalize :: Text -> Text
capitalize = T.toUpper

-- | Generate code for a literal
generateLiteral :: Literal -> Text
generateLiteral lit = case lit of
  LInt n -> T.pack (show n)
  LString s -> s
  LAtom a -> a

-- | Generate Erlang code and compile to BEAM with runner script
generateErlangFile :: FilePath -> Program -> IO ()
generateErlangFile outputFile program = do
  let moduleName = T.pack $ takeBaseName outputFile
  let erlangCode = generateErlangWithModule moduleName program
  writeFile outputFile (T.unpack erlangCode)
  compileErlang outputFile
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
  let scriptName = takeBaseName erlFile
  let scriptContent = unlines
        [ "#!/bin/bash"
        , "erl -noshell -pa . -s " ++ T.unpack moduleName ++ " main -s init stop"
        ]
  writeFile scriptName scriptContent
  callCommand $ "chmod +x " ++ scriptName
