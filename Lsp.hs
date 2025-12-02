{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lsp (runLspServer) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Language.LSP.VFS (virtualFileText)
import Language.LSP.Diagnostics (partitionBySource)
import qualified Language.LSP.Protocol.Lens as LSP
import Control.Lens ((^.))
import Parser (parseProgram, prettyError, hasMainFunction)
import System.IO (hPutStrLn, stderr, hFlush)

-- | Run the LSP server
runLspServer :: IO Int
runLspServer = do
  hPutStrLn stderr "[Amortia LSP] Starting server..."
  hFlush stderr
  runServer serverDefinition

-- | Server definition with handlers
serverDefinition :: ServerDefinition ()
serverDefinition = ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "amortia"
  , doInitialize = \env _req -> do
      liftIO $ hPutStrLn stderr "[Amortia LSP] Initializing..."
      liftIO $ hPutStrLn stderr "[Amortia LSP] Text sync: INCREMENTAL"
      liftIO $ hFlush stderr
      pure $ Right env
  , staticHandlers = \_caps -> handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = syncOptions
  }

-- | Sync options for text document synchronization
syncOptions :: Options
syncOptions = defaultOptions
  { optTextDocumentSync = Just $ TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Incremental
      , _willSave = Just False
      , _willSaveWaitUntil = Just False  
      , _save = Just $ InR $ SaveOptions { _includeText = Just False }
      }
  }

-- | LSP handlers
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_notification -> do
      liftIO $ hPutStrLn stderr "[Amortia LSP] ✓ Server initialized"
      liftIO $ hFlush stderr
  , notificationHandler SMethod_TextDocumentDidOpen $ \notification -> do
      let doc = notification ^. LSP.params . LSP.textDocument
          uri = doc ^. LSP.uri
          text = doc ^. LSP.text
      liftIO $ hPutStrLn stderr $ "[Amortia LSP] ✓ didOpen: " ++ show uri
      liftIO $ hFlush stderr
      validateDocument uri text
  , notificationHandler SMethod_TextDocumentDidChange $ \notification -> do
      let uri = notification ^. LSP.params . LSP.textDocument . LSP.uri
          changes = notification ^. LSP.params . LSP.contentChanges
      liftIO $ hPutStrLn stderr "================================================"
      liftIO $ hPutStrLn stderr $ "[Amortia LSP] ✓✓✓ didChange: " ++ show uri
      liftIO $ hPutStrLn stderr $ "[Amortia LSP] Number of changes: " ++ show (length changes)
      liftIO $ hFlush stderr
      
      mdoc <- getVirtualFile (toNormalizedUri uri)
      case mdoc of
        Just vf -> do
          let text = virtualFileText vf
          liftIO $ hPutStrLn stderr $ "[Amortia LSP] New content length: " ++ show (T.length text)
          liftIO $ hPutStrLn stderr "================================================"
          liftIO $ hFlush stderr
          validateDocument uri text
        Nothing -> do
          liftIO $ hPutStrLn stderr "[Amortia LSP] ✗ VFS lookup failed"
          liftIO $ hPutStrLn stderr "================================================"
          liftIO $ hFlush stderr
  , notificationHandler SMethod_TextDocumentDidSave $ \notification -> do
      let doc = notification ^. LSP.params . LSP.textDocument
          docUri = doc ^. LSP.uri
      liftIO $ hPutStrLn stderr $ "[Amortia LSP] ✓ didSave: " ++ show docUri
      liftIO $ hFlush stderr
      mdoc <- getVirtualFile (toNormalizedUri docUri)
      case mdoc of
        Just vf -> validateDocument docUri (virtualFileText vf)
        Nothing -> pure ()
  ]

-- | Validate a document and publish diagnostics
validateDocument :: Uri -> T.Text -> LspM () ()
validateDocument uri text = do
  liftIO $ hPutStrLn stderr $ "[Amortia LSP] Validating (" ++ show (T.length text) ++ " chars)"
  liftIO $ hFlush stderr
  
  let stripped = T.strip text
      isEmpty = T.null stripped
  
  diagnostics <- if isEmpty
    then do
      liftIO $ hPutStrLn stderr "[Amortia LSP] → Empty file, showing 'no entry point'"
      liftIO $ hFlush stderr
      pure [makeNoMainDiagnostic]
    else case parseProgram text of
      Left err -> do
        let errMsg = prettyError err
        liftIO $ hPutStrLn stderr "[Amortia LSP] → Parse ERROR"
        liftIO $ hFlush stderr
        pure [makeParseErrorDiagnostic errMsg]
      Right ast -> do
        liftIO $ hPutStrLn stderr "[Amortia LSP] → Parse OK"
        liftIO $ hFlush stderr
        if hasMainFunction ast
          then do
            liftIO $ hPutStrLn stderr "[Amortia LSP] → ✓ Has main - no errors"
            liftIO $ hFlush stderr
            pure []
          else do
            liftIO $ hPutStrLn stderr "[Amortia LSP] → Missing main function"
            liftIO $ hFlush stderr
            pure [makeNoMainDiagnostic]
  
  liftIO $ hPutStrLn stderr $ "[Amortia LSP] Publishing " ++ show (length diagnostics) ++ " diagnostic(s)"
  liftIO $ hFlush stderr
  publishDiagnostics 100 (toNormalizedUri uri) (Just 0) (partitionBySource diagnostics)

-- | Create a diagnostic for parse errors
makeParseErrorDiagnostic :: String -> Diagnostic
makeParseErrorDiagnostic errMsg = 
  let (range, cleanMsg) = extractRangeAndMessage errMsg
  in Diagnostic
    range
    (Just DiagnosticSeverity_Error)
    Nothing
    Nothing
    (Just "amortia")
    (T.pack cleanMsg)
    Nothing
    Nothing
    Nothing

-- | Create a diagnostic for missing main function
makeNoMainDiagnostic :: Diagnostic
makeNoMainDiagnostic = Diagnostic
  (Range (Position 0 0) (Position 0 0))
  (Just DiagnosticSeverity_Error)
  Nothing
  Nothing
  (Just "amortia")
  "No entry point found. Please define a main function: defn main :: () { ... }"
  Nothing
  Nothing
  Nothing

-- | Extract range and clean message from error
extractRangeAndMessage :: String -> (Range, String)
extractRangeAndMessage errMsg =
  let range = case parseErrorLocation errMsg of
        Just (line, col) ->
          let pos = Position (fromIntegral (line - 1)) (fromIntegral (col - 1))
          in Range pos pos
        Nothing -> Range (Position 0 0) (Position 0 0)
      cleanMsg = cleanErrorMessage errMsg
  in (range, cleanMsg)

-- | Parse error location from megaparsec error message
parseErrorLocation :: String -> Maybe (Int, Int)
parseErrorLocation msg =
  case lines msg of
    (firstLine:_) ->
      case break (== ':') firstLine of
        (_, ':':rest1) ->
          case break (== ':') rest1 of
            (lineStr, ':':rest2) ->
              case break (== ':') rest2 of
                (colStr, _) ->
                  case (reads lineStr, reads colStr) of
                    ([(line, "")], [(col, "")]) -> Just (line, col)
                    _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    [] -> Nothing

-- | Clean up error message for display
cleanErrorMessage :: String -> String
cleanErrorMessage msg =
  let ls = lines msg
  in case ls of
    [] -> "Parse error"
    (_:rest) -> 
      -- Skip location line, then filter out the visual markers (| N | and ^)
      let relevant = dropWhile null rest
          filtered = filter (not . isVisualMarker) relevant
      in if null filtered
           then "Parse error"
           else unlines filtered

-- | Check if a line is a visual marker line (| N | ... or just ^)
isVisualMarker :: String -> Bool
isVisualMarker line =
  let trimmed = dropWhile (== ' ') line
  in case trimmed of
    ('|':rest) -> 
      -- Check if it's like "| 9 |" or just "|"
      let afterPipe = dropWhile (== ' ') rest
      in case reads afterPipe :: [(Int, String)] of
        [(_, remaining)] -> 
          case dropWhile (== ' ') remaining of
            ('|':_) -> True
            _ -> False
        _ -> '|' `elem` trimmed
    ('^':_) -> True  -- Lines that are just carets
    _ -> False
