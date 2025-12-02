{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lsp (runLspServer)
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
  exitCode <- runLspServer
  exitWith $ if exitCode == 0 then ExitSuccess else ExitFailure exitCode
