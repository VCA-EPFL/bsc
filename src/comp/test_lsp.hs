{-# LANGUAGE OverloadedStrings, GADTs #-}
import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Monad
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens
import Language.LSP.Test
-- import Data.Default

import Control.Lens ((^.), to) -- Convenient
import Data.Aeson (ToJSON(toJSON))
-- main = runSession "/home/tbourgea/git/bsc-lsp/inst/bin/bsc_lsp" fullCaps "testsuite/lsp" $ do
main = runSessionWithConfig defaultConfig{ignoreLogNotifications=False, logMessages = True} "/home/tbourgea/git/bsc-lsp/inst/bin/bsc_lsp" fullCaps "testsuite/lsp" $ do
  -- Server is waiting for configuration from the client
  setConfigSection "demo" (toJSON @() ())
  rsp <-anyNotification

  -- liftIO $ print rsp
  doc <- openDoc "Test1.bsv" "bsv"
  liftIO $ print doc
  rsp <-anyNotification
  liftIO $ print rsp

  rsp <-anyNotification
  liftIO $ print rsp
  -- liftIO $ print rsp
  sendNotification SMethod_TextDocumentDidChange (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier (doc ^. uri ) 1)[])
  rsp <- anyNotification
  liftIO $ print rsp

  rsp <- anyNotification
  liftIO $ print rsp
  -- rsp <-anyNotification 
  -- liftIO $ print rsp
  -- liftIO $ putStrLn "Yo"
  -- rsp <- initializeResponse
  -- rsp <- anyMessage 
  -- liftIO $ print rsp
  -- liftIO $ print rsp
  -- rsp <-
    -- request SMethod_TextDocumentDocumentSymbol $
      -- DocumentSymbolParams Nothing Nothing doc
  -- liftIO $ print rsp
  -- liftIO $ putStrLn "Yo2"
  -- resp <- message SMethod_WindowLogMessage
  -- liftIO $ print resp
  -- -- Send requests and notifications and receive responses
  -- sendNotification SMethod_TextDocumentDidOpen $
  --     DidOpenTextDocumentParams doc

  -- Use your favourite combinators.
  -- skipManyTill loggingNotification (count 1 publishDiagnosticsNotification)
  -- rsp <-
    -- request SMethod_TextDocumentDocumentSymbol $
      -- DocumentSymbolParams Nothing Nothing doc
  -- liftIO $ print rsp

  -- Or use one of the helper functions
  -- getDocumentSymbols doc >>= liftIO . print
