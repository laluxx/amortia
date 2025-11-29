{-# LANGUAGE OverloadedStrings #-}

module Visualizer
  ( visualizeAST
  , watchAndVisualize
  ) where

import Parser
import System.Exit (exitFailure)
import System.Process (callCommand)
import qualified Data.Text.IO as TIO
import Data.Aeson (encode, object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when, unless)
import System.Directory (getModificationTime, doesFileExist)
import Data.Time (UTCTime)

-- | Simple string replacement
replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go str@(x:xs)
      | take (length old) str == old = new ++ go (drop (length old) str)
      | otherwise = x : go xs

-- | Generate HTML with embedded data
generateHTML :: FilePath -> FilePath -> IO (Maybe String)
generateHTML inputFile outputFile = do
  -- Read source code
  sourceText <- TIO.readFile inputFile

  -- Parse the file
  result <- parseFromFile inputFile
  case result of
    Left err -> do
      putStrLn $ "\ESC[1;31m✗\ESC[0m Parse error:\n" ++ err
      return Nothing
    Right ast -> do
      -- Read the HTML template
      htmlTemplate <- readFile "visualizer.html"

      -- Create JSON object with both source and AST
      let jsonData = object
            [ "source" .= sourceText
            , "ast" .= ast
            ]
      let jsonStr = TL.unpack . TLE.decodeUtf8 $ encode jsonData

      -- Replace placeholder with actual data
      let htmlContent = replace "/*DATA_PLACEHOLDER*/" jsonStr htmlTemplate

      -- Write to file
      writeFile outputFile htmlContent
      return (Just outputFile)

-- | Generate HTML and open in browser
visualizeAST :: FilePath -> IO ()
visualizeAST inputFile = do
  let outputFile = "/tmp/amortia-ast-view.html"

  result <- generateHTML inputFile outputFile
  case result of
    Nothing -> exitFailure
    Just file -> do
      putStrLn $ "\ESC[1;32m✓\ESC[0m Generated visualization: " ++ file
      putStrLn "Opening in browser..."

      -- Open in default browser
      callCommand $ "xdg-open " ++ file ++ " 2>/dev/null || open " ++ file

      putStrLn "\nPress Ctrl+C to exit"
      _ <- getLine
      return ()

-- | Watch file for changes and regenerate HTML
watchAndVisualize :: FilePath -> IO ()
watchAndVisualize inputFile = do
  let outputFile = "/tmp/amortia-ast-view.html"

  -- Check if file exists
  exists <- doesFileExist inputFile
  unless exists $ do
    putStrLn $ "\ESC[1;31m✗\ESC[0m File not found: " ++ inputFile
    exitFailure

  -- Initial generation
  result <- generateHTML inputFile outputFile
  case result of
    Nothing -> exitFailure
    Just file -> do
      putStrLn $ "\ESC[1;32m✓\ESC[0m Generated visualization: " ++ file
      putStrLn "Opening in browser..."
      callCommand $ "xdg-open " ++ file ++ " 2>/dev/null &"

      putStrLn "\n👁️  Watching for changes... (Ctrl+C to exit)\n"

      -- Watch loop
      lastModTime <- getModificationTime inputFile
      watchLoop inputFile outputFile lastModTime

-- | Watch loop that checks for file modifications
watchLoop :: FilePath -> FilePath -> UTCTime -> IO ()
watchLoop inputFile outputFile lastModTime = forever $ do
  threadDelay 500000  -- Check every 500ms

  exists <- doesFileExist inputFile
  when exists $ do
    currentModTime <- getModificationTime inputFile
    when (currentModTime > lastModTime) $ do
      putStrLn $ "♻️ Detected change in " ++ inputFile
      result <- generateHTML inputFile outputFile
      case result of
        Just _ -> putStrLn "\ESC[1;32m✓\ESC[0m Regenerated\n"
        Nothing -> putStrLn "\ESC[1;31m✗\ESC[0m Parse failed (fix errors and save)\n"
      watchLoop inputFile outputFile currentModTime
