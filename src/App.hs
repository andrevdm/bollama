{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module App where

import Verset
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick.Forms qualified as BFm
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.FileBrowser qualified as BFi
import Brick.Widgets.List qualified as BL
import Control.Concurrent.STM.TVar qualified as TV
import Control.Exception.Safe (catch)
import Data.Text.IO qualified as Txt
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty qualified as Vty
import System.Environment qualified as Env
import System.FilePath ((</>))

import Config qualified as Cfg
import Core qualified as C
import Draw qualified as D
import Events qualified as E
import Help qualified as H
import Logging qualified as L
import Messages qualified as M
import Storage.StoreImpl qualified as Sr
import Storage.Store qualified as Sr
import Theme qualified as T
import Widgets.PopupEditChat qualified as WPce

runTui :: IO ()
runTui = do
  Env.getArgs >>= \case
    ["--help"] -> showHelp
    [] -> runTuiMain
    ["helpMd"] -> do
      let t = H.renderHelpAsMarkdown H.helpContent
      putText t
    ["helpMd", path] -> do
      let t = H.renderHelpAsMarkdown H.helpContent
      Txt.writeFile path t
    _ -> do
      putText "Unknown argument"
      showHelp

  where
    showHelp = do
      putText $ "Bollama v" <> Cfg.verText <> "\n"
      putText "Usage: bollama [--help] [helpMd] [helpMd <path>]"
      putText "\n"



runTuiMain :: IO ()
runTuiMain = do
  eventChan <- BCh.newBChan 1000
  commandChan <- BCh.newBChan @C.Command 1000

  cfg <- Cfg.loadAppConfig

  dbPath' <- Cfg.getStateDir
  let dbPath = dbPath' </> "bollama.db"
  store <- Sr.newStoreWrapper $ Sr.newSqliteStore dbPath (\l e -> BCh.writeBChan commandChan $ C.CmdUpdateLog l e)
  store.swLog.lgInfo $ "Starting Bollama v" <> Cfg.verText

  -- Create a temporary chat
  _ <- store.swNewChat M.SsNotStreaming "#Temp" (Cfg.defaultModel cfg) M.emptyChatParams

  let runState = C.RunState
       { C.rsKilledChats = mempty
       }

  runState' <- TV.newTVarIO runState

  void . forkIO . catchEx store "commands channel" $ E.runCommands runState' cfg commandChan eventChan store
  void . forkIO . catchEx store "events channel" $ E.runTick eventChan

  (attrErrors, attrMap) <- Cfg.loadTheme

  when (not . null $ attrErrors) $
    store.swLog.lgWarn $ "Theme load errors: \n" <> Txt.unlines (attrErrors <&> ("  " <>))

  let app = B.App {
      B.appDraw = D.drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = E.handleEvent commandChan eventChan
    , B.appAttrMap = C._stAttrMap
    , B.appStartEvent = liftIO $ do
       BCh.writeBChan eventChan C.UeInit
       BCh.writeBChan commandChan C.CmdRefreshModelList
       BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Left <$> cfg.acDefaultChat)
    }

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty

  now <- DT.getCurrentTime
  let chatEditForm = WPce.mkPopChatEditForm M.emptyChatEditInfo


  exportBrowser <- BFi.newFileBrowser (const True) C.NPopExportBrowser (cfg.acDefaultExportDir)
  let exportBrowserDir = BFi.getWorkingDirectory exportBrowser

  let initialState = C.UiState
       { _stTick = 0
       , _stAppConfig = cfg
       , _stTab = cfg.acDefaultTab
       , _stNow = now
       , _stFooterMessage = Nothing
       , _stStore = store
       , _stLog = store.swLog
       , _stAttrMap = attrMap
       , _stErrorMessage = Nothing
       , _stShowThinking = False
       , _stShowMessageDetail = False
       , _stRunState = runState'

       , _stModels = []
       , _stModelsList = BL.list C.NModelsList mempty 1
       , _stModelsFilter = ""
       , _stModelListLoading = True
       , _stModelShowLoading = True
       , _stFocusModels = BF.focusRing [C.NModelsList]

       , _stPs = BL.list C.NListPs mempty 1
       , _stFocusPs = BF.focusRing [C.NListPs]
       , _stLoadingPs = True

       , _stFocusChat = BF.focusRing [C.NChatInputEdit, C.NChatsList]
       , _stChatInput = BE.editorText C.NChatInputEdit (Just 5) ""
       , _stChatCurrent = Nothing
       , _stChatMsgs = []
       , _stChatsList = BL.list C.NChatsList mempty 1

       , _stColoursList = BL.list C.NColoursList (V.fromList . sort $ fst <$> T.knownColours) 1

       , _stFocusLog = BF.focusRing [C.NLogList]
       , _stLogList = BL.list C.NLogList mempty 1
       , _stLogFilterIndex = 1

       , _stPopup = Nothing

       , _stPopChatEditFocus = BF.focusRing $ (BF.focusRingToList . BFm.formFocus $ chatEditForm) <> [C.NDialogOk, C.NDialogCancel]
       , _stPopChatEditTitle = Nothing
       , _stPopChatEditOnOk = (\_ _ _ -> pass)
       , _stPopChatEditForm = chatEditForm

       , _stPopPromptEdit = BE.editorText C.NPopPromptEdit (Just 1) ""
       , _stPopPromptTitle = Nothing
       , _stPopPromptFocus = BF.focusRing [C.NPopPromptEdit, C.NDialogOk, C.NDialogCancel]
       , _stPopPromptOnOk = const pass

       , _stPopConfirmFocus = BF.focusRing [C.NDialogCancel, C.NDialogOk]
       , _stPopConfirmTitle = Nothing
       , _stPopConfirmDetail = Nothing
       , _stPopConfirmOnOk = pass

       , _stPopContextTitle = Nothing
       , _stPopContextFocus = BF.focusRing [C.NPopContextList, C.NDialogOk, C.NDialogCancel]
       , _stPopContextList = BL.list C.NPopContextList mempty 1
       , _stPopContextOnOk = const pass

       , _stPopExportFocus = BF.focusRing [C.NPopExportBrowser, C.NPopExportDir, C.NPopExportFileName, C.NPopExportFormatJson, C.NPopExportFormatText, C.NDialogOk, C.NDialogCancel]
       , _stPopExportOnOk = const . const $ pass
       , _stPopExportBrowser = exportBrowser
       , _stPopExportDir = BE.editorText C.NPopExportDir (Just 1) (Txt.pack exportBrowserDir)
       , _stPopExportFName = BE.editorText C.NPopExportFileName (Just 1) ""
       , _stPopExportError = Nothing
       , _stPopExportFormat = M.ExportText
       }

  _finalState <- B.customMain @C.Name initialVty buildVty (Just eventChan) app initialState
  pass

  where
    catchEx :: Sr.StoreWrapper -> Text -> IO () -> IO ()
    catchEx store n action = do
      catch
        action
        (\(e :: SomeException) -> do
          store.swLog.lgError $ "Exception: in " <> n <> "\n" <> show e
        )


