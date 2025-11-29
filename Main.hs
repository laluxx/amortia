{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (callCommand, readProcess)
import qualified Data.Text.IO as TIO
import Data.Aeson (ToJSON, encode, encodeFile, object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
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

-- | Parse source file and output AST as JSON
parseToJSON :: FilePath -> FilePath -> IO ()
parseToJSON inputFile outputFile = do
  result <- parseFromFile inputFile
  case result of
    Right ast -> do
      encodeFile outputFile ast
      putStrLn $ "✓ Successfully parsed " ++ inputFile ++ " → " ++ outputFile
    Left err -> putStrLn $ "✗ Parse error:\n" ++ err

-- | Generate HTML with embedded data
generateHTML :: FilePath -> FilePath -> IO (Maybe String)
generateHTML inputFile outputFile = do
  -- Read source code
  sourceText <- TIO.readFile inputFile
  
  -- Parse the file
  result <- parseFromFile inputFile
  case result of
    Left err -> do
      putStrLn $ "✗ Parse error:\n" ++ err
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
      putStrLn $ "✓ Generated visualization: " ++ file
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
  when (not exists) $ do
    putStrLn $ "✗ File not found: " ++ inputFile
    exitFailure
  
  -- Initial generation
  result <- generateHTML inputFile outputFile
  case result of
    Nothing -> exitFailure
    Just file -> do
      putStrLn $ "✓ Generated visualization: " ++ file
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
      putStrLn $ "🔄 Detected change in " ++ inputFile
      result <- generateHTML inputFile outputFile
      case result of
        Just _ -> putStrLn "✓ Regenerated (refresh browser to see changes)\n"
        Nothing -> putStrLn "✗ Parse failed (fix errors and save again)\n"
      watchLoop inputFile outputFile currentModTime

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--watch", inputFile] -> watchAndVisualize inputFile
    ["--visualize", inputFile] -> visualizeAST inputFile
    ["--json", inputFile] -> parseToJSON inputFile "ast.json"
    ["--json", inputFile, outputFile] -> parseToJSON inputFile outputFile
    [inputFile] -> parseToJSON inputFile "ast.json"
    _ -> do
      putStrLn "Amortia Parser & Visualizer"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  amortia --watch <file>                 Watch file and auto-regenerate (hot reload)"
      putStrLn "  amortia --visualize <file>             Open AST visualizer in browser"
      putStrLn "  amortia --json <file> [output]         Generate JSON (default: ast.json)"
      putStrLn "  amortia <file>                         Generate ast.json"
      putStrLn ""
      putStrLn "Recommended: Use --watch for development!"
      exitFailure
