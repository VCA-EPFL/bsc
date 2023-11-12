{-# LANGUAGE CPP, OverloadedStrings, DuplicateRecordFields, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use mapMaybe" #-}
module Main_bsc_lsp(main, hmain) where

-- Haskell libs
import Prelude
import System.Environment(getProgName)
import System.FilePath(takeBaseName)
import CType
import ISyntax(IPackage(..))
import Error
import System.IO
    ( openFile,
      IOMode(AppendMode),
      stdout,
      hPutStr,
      stderr,
      hClose,
      hSetBuffering,
      BufferMode(LineBuffering),
      hSetEncoding,
      utf8 )
import System.Directory
    ( removeFile,
      getCurrentDirectory,
      createDirectoryIfMissing )
import Data.Maybe(isJust, fromJust, fromMaybe)

import PreStrings(fsEmpty)
import Control.Monad(when, foldM)
import qualified Data.Map as M

import Util(fromJustOrErr)

-- utility libs
import ParseOp
-- import PFPrint
import FileNameUtil(baseName, hasDotSuf, dropSuf, binSuffix,
                    bscSrcSuffix, bseSrcSuffix,
                    createEncodedFullFilePath)
import TopUtils
import IOUtil(getEnvDef)
-- import ISyntax(IPackage(..), IModule(..),
--                IEFace(..), IDef(..), IExpr(..), fdVars)
-- compiler libs
import FStringCompat
import Flags(
        Flags(..),
        DumpFlag(..),
        verbose)
import FlagsDecode(
        Decoded(..),
        decodeArgs,
        showFlags,
        showFlagsRaw)
-- import Error(ErrorHandle, ExcepWarnErr(..),
--              prEMsg, swarning, serror,
--              initErrorHandle, setErrorHandleFlags,
--              extractPosition,  bsWarning)
import Position(Position (..),
        getPositionLine,
        getPositionFile,
        mkPosition)
import Id
import Deriving(derive)
import MakeSymTab(mkSymTab, cConvInst)
import TypeCheck(cCtxReduceIO, cTypeCheck)
import BinUtil(BinMap, HashMap, readImports)
import GenBin(genBinFile)
import GenWrap(genWrap)
import GenFuncWrap(genFuncWrap, addFuncWrap)
import IExpandUtils(HeapData)

import Depend
import Version(bscVersionStr, copyright)
import Classic


import Language.LSP.Server
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Lens qualified as LSP
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (lift, liftIO)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format ( format, Only(Only) )
import Language.LSP.VFS qualified as VFS
import Control.Lens ((^.), to) -- Convenient
import Data.Aeson qualified as J
import GHC.IO(catchException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)

import Data.List qualified as L
import Control.Monad qualified as Monad
import PPrint qualified as P
import GHC.Generics (Generic)
-- import qualified PFPrint as Pp
import Data.Generics qualified as DataGenerics
import Data.Yaml qualified as Yaml
import Data.Either qualified as Either
import CSyntax
import GenSign(genUserSign, genEverythingSign)
import Control.Monad.Except (MonadError(catchError))
import Control.Exception (SomeException, try)


-- Current limitations: 
--  - Errors are at the line level (no multiline errors, no end of column error).
--  - bsc_lsp is singlethreaded
--  - TODO: Return the package and the name table as best as possible even if compilation fail (currently returns nothing)
--  - TODO: Get type (and so interface) information and not just definition (SMethod_TextDocumentSignatureHelp) 
--  - TODO: Support go to [instance.subinterface1.subsub.method]  in general this is very
--     difficult. Already instance.method() is tricky as we would need to know
--     the line where method is defined -> Maybe special case it as it would be
--     quite useful? -> Call the frontend for the subfile to find the position
--     of the method. Investigate if we can interact between the js extension and the haskell
--  - TODO [getWorkspace] is a terrible misleading name that I picked. Must change it


deriving instance DataGenerics.Data CTypeclass
deriving instance DataGenerics.Data CPred
deriving instance DataGenerics.Data PartialKind
deriving instance DataGenerics.Data CQType
deriving instance DataGenerics.Data CDefn
deriving instance DataGenerics.Data IdK
deriving instance DataGenerics.Data CExpr
deriving instance DataGenerics.Data CLiteral
deriving instance DataGenerics.Data COp
deriving instance DataGenerics.Data CInternalSummand
deriving instance DataGenerics.Data COriginalSummand
deriving instance DataGenerics.Data CField
deriving instance DataGenerics.Data CCaseArm
deriving instance DataGenerics.Data CStmt
deriving instance DataGenerics.Data CMStmt
deriving instance DataGenerics.Data CRule
deriving instance DataGenerics.Data CDefl
deriving instance DataGenerics.Data CDef
deriving instance DataGenerics.Data CClause
deriving instance DataGenerics.Data CQual
deriving instance DataGenerics.Data CPat
deriving instance DataGenerics.Data CPOp

deriving instance DataGenerics.Typeable CTypeclass
deriving instance DataGenerics.Typeable CPred
deriving instance DataGenerics.Typeable PartialKind
deriving instance DataGenerics.Typeable CQType
deriving instance DataGenerics.Typeable CDefn
deriving instance DataGenerics.Typeable IdK
deriving instance DataGenerics.Typeable CExpr
deriving instance DataGenerics.Typeable CLiteral
deriving instance DataGenerics.Typeable COp
deriving instance DataGenerics.Typeable CInternalSummand
deriving instance DataGenerics.Typeable COriginalSummand
deriving instance DataGenerics.Typeable CField
deriving instance DataGenerics.Typeable CCaseArm
deriving instance DataGenerics.Typeable CStmt
deriving instance DataGenerics.Typeable CMStmt
deriving instance DataGenerics.Typeable CRule
deriving instance DataGenerics.Typeable CDefl
deriving instance DataGenerics.Typeable CDef
deriving instance DataGenerics.Typeable CClause
deriving instance DataGenerics.Typeable CQual
deriving instance DataGenerics.Typeable CPat
deriving instance DataGenerics.Typeable CPOp

definedNames :: CDefn -> [Id]
definedNames x = either (const  []) (: []) $ getName x

getPreviousPosFromParsedPackage :: Id -> CPackage -> [Id]
getPreviousPosFromParsedPackage pos (CPackage _ _ _ _ defns _) =
    concatMap (pruneLocalCDefn pos) defns


collectPositionsCDefn :: CDefn -> [Position]
collectPositionsCDefn = DataGenerics.listify isP
  where isP p = True


within :: Int -> [ Position ] -> Bool
within line [] = False
within line l =
    line >= minimum l' && line <= maximum l'
    where
        l' = L.filter (/= -1) $ getPositionLine <$> l

-- Ids1, Ids2 and Ids3 are to focus only on definition/redefinitions one per type that can define stuff
collectIds1 :: CDefn -> [Id]
collectIds1 = DataGenerics.everything
    (++)
    (DataGenerics.mkQ [] test)
    --No need to do CDefT as we are pretypechecking?
    where
        test cdef@(CDef id _ _) = [id]
        test _ = []

collectIds2 :: CDefn -> [Id]
collectIds2 = DataGenerics.everything
    (++)
    (DataGenerics.mkQ [] test)
    where
        test cdef@(CPVar x) = [x]
        test _ = []

collectIds3 :: CDefn -> [Id]
collectIds3 = DataGenerics.everything
    (++)
    (DataGenerics.mkQ [] test)
    where
        test cdef@(CLValue x _ _) = [x]
        test _ = []


pruneLocalCDefn :: Id -> CDefn  -> [Id]
pruneLocalCDefn pos cdefn =
    -- Naive implementation here, we do several times the same traversal (1 for position + 3 times for different kinds of ids)
    -- We contextualize a bit by scoping the search to the current toplevel definition only, but we could still get cross rules pollutions.
    if within (getPositionLine $ getPosition pos) $ collectPositionsCDefn cdefn then
        case cdefn of
            CValueSign cdef ->
                    L.filter (== pos) $ collectIds1 cdefn ++ collectIds2 cdefn ++ collectIds3 cdefn
            _ -> []
    else
       []


data Config = Config {projectFile :: FilePath }
    deriving (Generic, J.ToJSON, J.FromJSON, Show)

data ProjectConfig = ProjectConfig { bscExtraArgs :: [String], bscUserDirs :: [String]}
    deriving (Generic, Show, J.ToJSON, J.FromJSON)

defaultProjectConfig :: ProjectConfig
defaultProjectConfig = ProjectConfig { bscExtraArgs = [], bscUserDirs = []}

pathBsvLibs :: ProjectConfig -> String
pathBsvLibs cfg = Monad.join . L.intersperse ":" $ bscUserDirs cfg ++ ["+"]

-- Every options passed to Bluespec except the filename, and [-u] in the case
-- where we compile recursively
commandOptions :: ProjectConfig -> Config -> String -> [String]
commandOptions pcfg cfg builddir = ["--aggressive-conditions"] ++ bscExtraArgs pcfg ++ ["-p", pathBsvLibs pcfg, "-bdir", builddir ++ "/"]

logForClient :: MonadLsp config f => T.Text -> f ()
logForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Log x

errorForClient :: MonadLsp config f => T.Text -> f ()
errorForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Error x

diagsForClient :: MonadLsp config f => LSP.Uri -> [LSP.Diagnostic] -> f ()
diagsForClient doc diags = sendNotification LSP.SMethod_TextDocumentPublishDiagnostics $ LSP.PublishDiagnosticsParams doc Nothing diags

-- We modified the error handling of the compiler to generate an exception [ExcepWarnErr] on warnings and errors
-- This function transform such an exception into LSP diagnostics
diagFromExcep  :: ExcepWarnErr -> ([LSP.Diagnostic], [LSP.Diagnostic], Maybe a, Maybe b)
diagFromExcep (ExcepWarnErr err warn ctxt) = do
    let diag =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Error)
                                    Nothing Nothing Nothing
                                    (T.pack . P.pretty 78 78 . prEMsg serror ctxt $ x)
                                    Nothing Nothing Nothing ) err
        diagw =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Warning)
                                    Nothing Nothing Nothing
                                    (T.pack . P.pretty 78 78 . prEMsg swarning ctxt $ x)
                                    -- (T.pack $ show $ extractMessage x)
                                    Nothing Nothing Nothing) warn
    (diag, diagw, Nothing, Nothing)

emptyDiags :: (Bool, (BinMap HeapData), p, p, ErrorHandle) -> IO ([LSP.Diagnostic], [LSP.Diagnostic], Maybe (BinMap HeapData), Maybe p)
emptyDiags (_, binmap, packagetc, package,ref) = do
    finalWarns <- lspFinalWarns <$> readErrorState ref
    ctxt <- lspContext <$> readErrorState ref
    let diagw = map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Warning)
                                    Nothing Nothing Nothing
                                    (T.pack . P.pretty 78 78 . prEMsg swarning ctxt $ x)
                                    Nothing Nothing Nothing) finalWarns
    return ([], diagw, Just binmap, Just package)

withFilePathFromUri :: MonadLsp config f => LSP.Uri -> (FilePath -> f ()) -> f ()
withFilePathFromUri uri k =
    case LSP.uriToFilePath uri of
        Just s -> k s
        Nothing -> logForClient . toStrict $ format "URI {} cannot be transformed into Filepath" (Only (show uri))

updateNametable :: LSP.Uri -> Maybe (BinMap HeapData) -> Maybe CPackage-> LspState ()
updateNametable doc maybepackage cpackage =
    -- This function assumes that if [maybepackage] is Nothing, then [cpackage]
    -- is also Nothing (Both datastructure should be Nothing simultaneously)
    Monad.forM_ cpackage (\c ->
        Monad.forM_ maybepackage (\m -> do
                        stRef <- lift ask
                        let defs_public_aux = L.map (\(x,y,z,t,u) -> y) $ M.elems m
                            defs_public = L.concatMap (\(CSignature _namePackage _imported _fixity defs) ->
                                                        L.concatMap
                                                            definedNames
                                                            defs) defs_public_aux
                            fulldefs_local = (\(CPackage _ _ _ _ defns _) -> L.concatMap definedNames defns) c
                            all_defs = fulldefs_local ++ defs_public -- Might contain duplicate
                        liftIO . modifyMVar_ stRef $ \x ->
                                                    return $ x{visible_global_identifiers = M.insert
                                                                    doc all_defs (visible_global_identifiers x),
                                                                parsedByUri = M.insert doc c (parsedByUri x)}))

type LspState = LspT Config (ReaderT (MVar ServerState) IO)

data ServerState = ServerState {
    -- Currently easy implementation for global identifiers, To avoid
    -- ambiguities of definitions because different files could use different
    -- subpackages that could import the same identifier we keep one global Id
    -- position table per Uri defined

    -- TODO: UI question - what to do when file does not compile, should we 
    -- keep the old mapping or delete it? 
    -- Currently leaning toward keeping the old mapping.
    visible_global_identifiers :: M.Map LSP.Uri [Id],
    -- Following currently unused
    typeId :: M.Map LSP.Uri (M.Map Id Id),
    parsedByUri :: M.Map LSP.Uri CPackage,
    buildDir :: FilePath,
    projectConfig :: ProjectConfig,
    bDir:: FilePath

    -- We are missing the definitions here

    -- In the future, this type should carry:
    -- TODO Add documentations 
    -- TODO Add Uri -> NonGlobalDefinitions even for broken files
    -- TODO Add all references to Id? Id -> [(Uri, Position)]
    -- TODO Type definitions vs value definitions.
}

idAtPos :: LSP.Position -> T.Text -> Maybe (Int, (Int, Int), T.Text)
idAtPos pos text =
    -- the id is necessarily on a single line first extract the line
    let line_n :: Int = fromIntegral $ pos ^. LSP.line
        column_n :: Int = fromIntegral $ pos ^. LSP.character
        line = T.lines text L.!! line_n in
    (\(x,y,z) -> (line_n, (x,y), z)) <$> extractWordAroundPosition column_n line

extractWordAroundPosition :: Int -> T.Text -> Maybe (Int, Int, T.Text)
extractWordAroundPosition position text = case findWordBoundaries position text of
    Just (start, end) -> Just (start, end, T.take (end - start) (T.drop start text))
    Nothing           -> Nothing

findWordBoundaries :: Int -> T.Text -> Maybe (Int, Int)
findWordBoundaries position text = do
    let (before, after) = T.splitAt position text
    let start = T.length (T.takeWhile isWordChar (T.reverse before))
    let end = T.length (T.takeWhile isWordChar after) + position
    return (position - start, end)

isWordChar :: Char -> Bool
isWordChar c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9'] || c `elem` ['_']

bscPosToLSPPos :: (Int, (Int,Int)) -> Position -> LSP.DefinitionLink
bscPosToLSPPos (l ,(start,end)) pos =
    let line_target =  fromIntegral $ getPositionLine pos - 1
        range = LSP.Range (LSP.Position line_target 0) (LSP.Position line_target 1000) in
    LSP.DefinitionLink $ LSP.LocationLink
        (Just . LSP.Range (LSP.Position (fromIntegral l) (fromIntegral start)) $ LSP.Position (fromIntegral l) (fromIntegral end))
        (LSP.filePathToUri $ getPositionFile pos)
        range
        range --(LSP.filePathToUri $ getPositionFile pos) _

getWorkspace :: LspState FilePath
getWorkspace = do
            stRef <- lift ask
            servState <- liftIO $ readMVar stRef
            return $ buildDir servState

processPaths :: FilePath -> ProjectConfig -> ProjectConfig
processPaths root pcfg = pcfg{ bscUserDirs = appendRelative $ bscUserDirs pcfg }
    where
        appendRelative [] = []
        appendRelative (t:q) =
            if length t == 0
                then appendRelative q
                else
                    if head t == '/'
                        then t : appendRelative q
                        else (root ++ "/" ++ t) : appendRelative q

handlers :: Handlers LspState
handlers =
  mconcat
-- TODO Add handler for contextual completion (should be easy?)
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        cfg <- getConfig
        -- Read the Yaml file
        root <- getRootPath
        stRef <- lift ask

        bdir <- liftIO $ getEnvDef "BLUESPECDIR" dfltBluespecDir
        case root of
         Just root -> do
            (yaml :: Either.Either Yaml.ParseException ProjectConfig) <- liftIO . Yaml.decodeFileEither $ root ++ "/" ++ projectFile cfg
            logForClient . toStrict $ format "Trying to read the yaml file {} {} bdir {}" ( root ++ "/" ++ projectFile cfg, show yaml, bdir)
            let pcfg_aux = Either.fromRight defaultProjectConfig yaml
                pcfg = processPaths root pcfg_aux
                workspace = root ++ "/.bsclsp"
            liftIO $ createDirectoryIfMissing True workspace
            liftIO . modifyMVar_ stRef $ \x -> return $ x{buildDir= workspace, projectConfig = pcfg, bDir = bdir}
            logForClient . toStrict $ format "BSC LSP Server Initialized {} workspace {} " (show cfg, root)
         Nothing -> do
            logForClient . toStrict $ "No root found - defaulting to empty yaml file"
            let workspace = "/tmp/.globalbsclsp"
            liftIO $ createDirectoryIfMissing True workspace
            liftIO . modifyMVar_ stRef $ \x -> return $ x{buildDir= workspace, projectConfig= defaultProjectConfig, bDir = bdir}
    , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_dummy -> do
        -- See Note about LSP configuration in the haskell lsp package
        return ()
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text  -- Extract content first time file is opened
        cfg <- getConfig
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            logForClient . toStrict $ format  "Open File {} " (Only file)
            stRef <- lift ask
            pcfg <- liftIO $ projectConfig <$> readMVar stRef
            -- We first delete the BO because we will regenerate it if possible
            _ <- liftIO (try (removeFile (workspace ++"/" ++ takeBaseName file ++ ".bo")) :: IO (Either SomeException ()))
            (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (hmain (commandOptions pcfg cfg workspace ++ [file])  (T.unpack content) True >>= emptyDiags) $
                                                return . diagFromExcep
            diagsForClient doc $ diagErrs ++ diagWarns
            updateNametable doc maybeNameTable maybePackage)
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do -- VFS automatically contains the content
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
                Just vf@(VFS.VirtualFile _ version rope) -> do
                    stRef <- lift ask
                    pcfg <- liftIO $ projectConfig <$> readMVar stRef
                    (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (hmain (commandOptions pcfg cfg workspace ++ [file])  (T.unpack (VFS.virtualFileText vf)) False >>= emptyDiags) $
                                                return . diagFromExcep
                    diagsForClient doc (diagErrs ++ diagWarns)
                    updateNametable doc maybeNameTable maybePackage
                Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                stRef <- lift ask
                pcfg <- liftIO $ projectConfig <$> readMVar stRef
                _ <- liftIO (try (removeFile (workspace ++"/" ++ takeBaseName file ++ ".bo")) :: IO (Either SomeException ()))
                (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (hmain (commandOptions pcfg cfg workspace ++ [file])  (T.unpack (VFS.virtualFileText vf)) True >>= emptyDiags) $
                                                return . diagFromExcep
                diagsForClient doc (diagErrs ++ diagWarns)
                updateNametable doc maybeNameTable maybePackage
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
        let pos = req ^. LSP.params . LSP.position -- pos is a position in the document
            doc = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                    stRef <- lift ask
                    serverState <- liftIO $ readMVar stRef
                    case M.lookup doc $ visible_global_identifiers serverState of
                        Just ids -> do
                            let add_key = (\id -> (getIdBase id, id)) <$> ids
                                searched = idAtPos pos (VFS.virtualFileText vf)
                                (line, (colbegin,_colend), idname) = fromMaybe (-1, (-1,-1), "_unused_42") $ searched
                                find_ids_glob = snd <$> L.filter (\(key,v) -> toString key == T.unpack idname) add_key
                                find_ids_local = getPreviousPosFromParsedPackage (mkId (mkPosition fsEmpty line colbegin ) .  fromString $ T.unpack idname) .
                                                    fromMaybe (CPackage undefined undefined undefined undefined [] undefined) $ M.lookup doc $ parsedByUri serverState
                                find_ids = find_ids_local ++ find_ids_glob
                            responder . Right $ LSP.InR $ LSP.InL $ bscPosToLSPPos ((\(l,(cs,ce),z)-> (l,(cs,ce))) $ fromJust searched) <$> getIdPosition  <$> find_ids
                        Nothing ->
                            responder . Right $ LSP.InR $ LSP.InL []
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))

    , requestHandler LSP.SMethod_TextDocumentSignatureHelp $ \req responder -> do
        -- TODO
        let _pos = req ^. LSP.params . LSP.position -- pos is a position in the document
            doc = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                    stRef <- lift ask
                    serverState <- liftIO $ readMVar stRef
                    return ()
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))

    , notificationHandler LSP.SMethod_TextDocumentDidClose $ \msg -> do -- Check what is being written
        let _doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        -- TODO: free the data corresponding to the file?
        return ()

    -- TODO: Don't know what SetTrace is used for, but vscode keeps sending
    -- those event, so we make a dummy handler.
    , notificationHandler LSP.SMethod_SetTrace $ \msg -> do
        return ()
    ]



initialServerState :: IO (MVar ServerState)
initialServerState = do
    newMVar ServerState{ visible_global_identifiers = M.empty,
    buildDir = "/tmp/.globalbsclsp",
    typeId = M.empty,
    parsedByUri = M.empty,
    projectConfig = defaultProjectConfig,
    bDir = ""}

runLSPAndState :: LspState a  -> MVar ServerState -> LanguageContextEnv Config ->  IO a
runLSPAndState lsp state env = runReaderT (runLspT env lsp) state

main :: IO Int
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    state <- initialServerState
    runServer $
        ServerDefinition { parseConfig = \_old new -> case J.fromJSON new of
                                                            J.Success v -> Right v
                                                            J.Error err -> Left $ T.pack err
                         , onConfigChange = const $ pure ()
                         , defaultConfig = Config {projectFile = "" }
                         , configSection = "glspc.initializationOptions" -- TODO investigate what this configSection is suppose to do
                         , doInitialize = \env _req -> pure $ Right env
                         , staticHandlers = const handlers
                        --  , interpretHandler = \env -> Iso (runLspT env) liftIO
                         , interpretHandler = \env -> Iso (\lsp -> runLSPAndState lsp state env) liftIO
                         , options = defaultOptions { -- set sync options to get DidSave event, as well as Open and Close events.
                                             optTextDocumentSync =
                                               Just
                                                 ( LSP.TextDocumentSyncOptions
                                                     (Just True)
                                                     (Just LSP.TextDocumentSyncKind_Full) -- Easier for now, could try to play with incremental?
                                                     (Just False)
                                                     (Just False)
                                                     (Just . LSP.InR . LSP.SaveOptions $ Just True)
                                                 ) }
                         }


-- Use with hugs top level
-- hmain :: [String] -> String -> IO Bool
hmain :: [String] -> String -> Bool -> IO (Bool, BinMap HeapData, CPackage, CPackage, ErrorHandle)
hmain args contentFile createBo = do
    pprog <- getProgName
    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    -- reconstruct original command line (modulo whitespace)
    -- add a newline at the end so it is offset
    let cmdLine = concat ("Invoking command line:\n" : (L.intersperse " " (pprog:args'))) ++ "\n"
    let showPreamble flags = do
          when (verbose flags) $ putStrLnF (bscVersionStr True)
          when (verbose flags) $ putStrLnF copyright
          when ((verbose flags) || (printFlags flags)) $ putStrLnF cmdLine
          when ((printFlags flags) || (printFlagsHidden flags)) $
                putStrLnF (showFlags flags)
          when (printFlagsRaw flags) $ putStrLnF (showFlagsRaw flags)
    let (warnings, decoded) = decodeArgs (baseName pprog) args' cdir
    errh <- initErrorHandle True
    let doWarnings = when ((not . null) warnings) $ bsWarning errh warnings
        setFlags = setErrorHandleFlags errh
    case decoded of
        DBlueSrc flags src ->
            do { setFlags flags; doWarnings; showPreamble flags;
                 (x,y,z,t) <- main' errh flags src contentFile createBo;
                 return (x,y,z,t, errh)
                }
        _ -> error "Internal error bsc_lsp"


-- main' :: ErrorHandle -> Flags -> String -> String -> IO Bool
main' :: ErrorHandle -> Flags -> String -> String -> Bool -> IO (Bool, BinMap HeapData, CPackage, CPackage)
main' errh flags name contentFile createBo =  do
    setErrorHandleFlags errh flags
    tStart <- getNow

    -- Quick compile of submodules
    try (compile_with_deps errh flags name) :: IO (Either SomeException ())
    let comp = compile
    comp errh flags name contentFile createBo

compile_with_deps :: ErrorHandle -> Flags -> String -> IO ()
compile_with_deps errh flags name = do
    fs <- filter (/= name) <$> chkDeps errh flags name
    errh' <- initErrorHandle True
    let
        -- verb = showUpds flags && not (quiet flags)
        -- the flags to "compileFile" when re-compiling depended modules
        flags_depend = flags { updCheck = False,
                               genName = [],
                               showCodeGen = False }
        -- the flags to "compileFile" when re-compiling this module
        comp (success, binmap0, hashmap0) fn = do
            file <- doCPP errh flags fn
            let fl = flags_depend
            (cur_success, binmap, hashmap, cpackage, parsed)
                <- compileFile errh' fl binmap0 hashmap0 fn file True
            return (cur_success && success, binmap, hashmap)
    -- quick compile them
    _ <- foldM comp (True, M.empty, M.empty) fs
    return ()

-- compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO Bool
compile :: ErrorHandle -> Flags -> String -> String -> Bool -> IO (Bool, BinMap HeapData, CPackage, CPackage)
compile errh flags name contentFile createBo = do
  (ok, loaded, _, cpackage, parsed) <- compileFile errh flags M.empty M.empty name contentFile createBo -- Pass the string that contains the thing to tc
  return (ok, loaded, cpackage, parsed)

-- read_and_compile :: ErrorHandle -> Flags -> String -> String -> IO (Bool, BinMap HeapData, CPackage, CPackage)
-- read_and_compile errh flags name fileName = do
--   let contentFile = undefined
--   (ok, loaded, _, cpackage, parsed) <- compileFile errh flags M.empty M.empty name contentFile True -- Pass the string that contains the thing to tc
--   return (ok, loaded, cpackage, parsed)

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String -> String -> Bool ->
               IO (Bool, BinMap HeapData, HashMap, CPackage, CPackage)
compileFile errh flags binmap hashmap name_orig file createBo = do
    pwd <- getCurrentDirectory
    let name = (createEncodedFullFilePath name_orig pwd)

    let syntax = (if      hasDotSuf bscSrcSuffix name then CLASSIC
                  else if hasDotSuf bseSrcSuffix name then ESE
                  else BSV)
    setSyntax syntax

    t <- getNow
    -- ===== the break point between file manipulation and compilation
    (pkg@(CPackage i _ imports _ _ _), t)
        <- parseSrc (syntax == CLASSIC) errh flags True name file

    let dumpnames = (baseName (dropSuf name), getIdString (unQualId i), "")
    compilePackage errh flags dumpnames t binmap hashmap name pkg createBo

-------------------------------------------------------------------------

compilePackage ::
    ErrorHandle ->
    Flags ->
    DumpNames ->
    TimeInfo ->
    BinMap HeapData ->
    HashMap ->
    String ->
    CPackage ->
    Bool ->
    IO (Bool, BinMap HeapData, HashMap, CPackage, CPackage)
compilePackage
    errh
    flags                -- user switches
    dumpnames
    tStart
    binmap0
    hashmap0
    name -- String --
    min@(CPackage pkgId _ _ _ _ _)
    createBo = do
    -- Read imported signatures
    (mimp@(CPackage _ _ imps _ _ _), binmap, hashmap)
        <- readImports errh flags binmap0 hashmap0 min

    -- [T]: binmap contains all the submodules that we care about
    -- Can we know which local identifiers point to what?
    mop <- parseOps errh mimp

    -- Generate a global symbol table
    --
    -- Later stages will introduce new symbols that will need to be added
    -- to the symbol table.  Rather than worry about properly inserting
    -- the new symbols, we just build the symbol table fresh each time.
    -- So this is the first of several times that the table is built.
    -- We can't delay the building of the table until after all symbols
    -- are known, because these next stages need a table of the current
    -- symbols.
    --
    symt00 <- mkSymTab errh mop

    -- whether we are doing code generation for modules
    let generating = isJust (backend flags)

    (mfwrp, symt0, funcs) <- genFuncWrap errh flags generating mop symt00

    -- Generate wrapper for Verilog interface.
    (mwrp, gens) <- genWrap errh flags (genName flags) generating mfwrp symt0

    -- Rebuild the symbol table because GenWrap added new types
    -- and typeclass instances for those types
    symt1 <- mkSymTab errh mwrp

    -- Re-add function definitions for `noinline'
    mfawrp <- addFuncWrap errh symt1 funcs mwrp

    -- Turn deriving into instance declarations
    mder <- derive errh flags symt1 mfawrp

    -- Rebuild the symbol table because Deriving added new instances
    symt11 <- mkSymTab errh mder

    -- Reduce the contexts as far as possible
    mctx <- cCtxReduceIO errh flags symt11 mder

    -- Rebuild the symbol table because CtxReduce has possibly changed
    -- the types of top-level definitions
    symt <- mkSymTab errh mctx

    -- Turn instance declarations into ordinary definitions
    let minst = cConvInst errh symt mctx

    -- Type check and insert dictionaries
    start flags DFtypecheck
    (mod, tcErrors) <- cTypeCheck errh flags symt minst

    -- Need to compute the signature of the imports imports (imps)
    let (_, _, _, binmods0, pkgsigs) =
            let findFn i = fromJustOrErr "bsc: binmap" $ M.lookup i binmap
                sorted_ps = [ getIdString i
                               | CImpSign _ _ (CSignature i _ _ _) <- imps ]
            in  L.unzip5 $ map findFn sorted_ps
    let
        -- adjust the "raw" packages and then add back their signatures
        -- so they can be put into the current IPackage for linking info
        -- binmods = zip (map (adjEnv env) binmods0) pkgsigs
        binmods = zip binmods0 pkgsigs

    let ipkg_sigs = [ (mi, s) | (m@(IPackage mi _ _ _), s) <- binmods]

    -- handle <- openFile "/tmp/output.txt" AppendMode
    -- liftIO $ hPutStr handle $ show y
    -- liftIO $ hClose handle

    -- Generate the user-visible type signature
    bi_sig <- genUserSign errh symt mctx
    -- Generate a type signature where everything is visible
    bo_sig <- genEverythingSign errh symt mctx
    let bin_filename = putInDir (bdir flags) name binSuffix
    when createBo $ genBinFile errh bin_filename bi_sig bo_sig (IPackage {
              -- package name
              ipkg_name = pkgId,
              ipkg_depends = ipkg_sigs, -- Need to put the dependences here (name signature)
              ipkg_pragmas = [],
              ipkg_defs = []
          })
    return ( not tcErrors, binmap, hashmap, mod, min)