{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Codegen
import Visualizer
import Lsp (runLspServer)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import Data.Aeson (encodeFile)
import System.FilePath (replaceExtension)

-- | Parse source file and generate Erlang code with BEAM compilation
parseAndGenerate :: FilePath -> FilePath -> IO ()
parseAndGenerate inputFile outputFile = do
  result <- parseFromFile inputFile
  case result of
    Right ast -> do
      -- Check if main function exists
      if hasMainFunction ast
        then do
          generateErlangFile outputFile ast
          putStrLn $ "\ESC[1;32m✓\ESC[0m Successfully compiled " ++ inputFile ++ " \ESC[1;34m→\ESC[0m " ++ outputFile
        else do
          putStrLn "\ESC[1;31m✗\ESC[0m Compilation error: No entry point found"
          putStrLn "   Please define a main function with signature: defn main :: () { ... }"
          exitFailure
    Left err -> do
      putStrLn $ "\ESC[1;31m✗\ESC[0m Parse error:\n" ++ err
      exitFailure

-- | Parse source file and output AST as JSON
parseToJSON :: FilePath -> FilePath -> IO ()
parseToJSON inputFile outputFile = do
  result <- parseFromFile inputFile
  case result of
    Right ast -> do
      encodeFile outputFile ast
      putStrLn $ "\ESC[1;32m✓\ESC[0m Successfully parsed " ++ inputFile ++ " \ESC[1;34m→\ESC[0m " ++ outputFile
    Left err -> putStrLn $ "\ESC[1;31m✗\ESC[0m Parse error:\n" ++ err

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lsp"] -> do
      exitCode <- runLspServer
      exitWith $ if exitCode == 0 then ExitSuccess else ExitFailure exitCode
    ["watch", inputFile] -> watchAndVisualize inputFile
    ["visualize", inputFile] -> visualizeAST inputFile
    ["json", inputFile] -> parseToJSON inputFile "ast.json"
    ["json", inputFile, outputFile] -> parseToJSON inputFile outputFile
    [inputFile] -> parseAndGenerate inputFile (replaceExtension inputFile ".erl")
    [inputFile, outputFile] -> parseAndGenerate inputFile outputFile
    _ -> do
      putStrLn "Amortia Compiler & Visualizer"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  amortia <file>                         Compile to BEAM executable"
      putStrLn "  amortia <file> <output>                Compile with custom output name"
      putStrLn "  amortia json <file> [output]           Generate AST as JSON"
      putStrLn "  amortia watch <file>                   Watch file and auto-regenerate (hot reload)"
      putStrLn "  amortia visualize <file>               Open AST visualizer in browser"
      putStrLn "  amortia lsp                            Start LSP server"
      putStrLn ""
      exitFailure
