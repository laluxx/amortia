{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Codegen
import Visualizer
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Aeson (encodeFile)
import System.FilePath (replaceExtension)

-- | Parse source file and generate Erlang code
parseAndGenerate :: FilePath -> FilePath -> IO ()
parseAndGenerate inputFile outputFile = do
  result <- parseFromFile inputFile
  case result of
    Right ast -> do
      generateErlangFile outputFile ast
      putStrLn $ "\ESC[1;32m✓\ESC[0m Successfully compiled " ++ inputFile ++ " \ESC[1;34m→\ESC[0m " ++ outputFile
    Left err -> putStrLn $ "\ESC[1;31m✗\ESC[0m Parse error:\n" ++ err

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
    ["watch", inputFile] -> watchAndVisualize inputFile
    ["visualize", inputFile] -> visualizeAST inputFile
    ["json", inputFile] -> parseToJSON inputFile "ast.json"
    ["json", inputFile, outputFile] -> parseToJSON inputFile outputFile
    ["erl", inputFile] -> parseAndGenerate inputFile (replaceExtension inputFile ".erl")
    ["erl", inputFile, outputFile] -> parseAndGenerate inputFile outputFile
    [inputFile] -> parseAndGenerate inputFile (replaceExtension inputFile ".erl")
    _ -> do
      putStrLn "Amortia Compiler & Visualizer"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  amortia <file>                         Compile to Erlang (output: .erl)"
      putStrLn "  amortia erl <file> [output]            Compile to Erlang with custom output"
      putStrLn "  amortia json <file> [output]           Generate AST as JSON"
      putStrLn "  amortia watch <file>                   Watch file and auto-regenerate (hot reload)"
      putStrLn "  amortia visualize <file>               Open AST visualizer in browser"
      putStrLn ""
      exitFailure
