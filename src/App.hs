{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module App where

import           Verset

import Brick.AttrMap qualified as BA
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Exception.Safe (catch)
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform qualified as Vty
import System.FilePath ((</>))

import Core qualified as C
import Config qualified as Cfg
import Draw qualified as D
import Events qualified as E
import Storage qualified as Sr
import Utils qualified as U


runTui :: IO ()
runTui = do
  eventChan <- BCh.newBChan 1000
  commandChan <- BCh.newBChan @C.Command 1000

  cfg <- Cfg.loadAppConfig

  dbPath' <- Cfg.getStateDir
  let dbPath = dbPath' </> "bollama.db"
  store <- Sr.newStoreWrapper $ Sr.newSqliteStore dbPath (\l e -> BCh.writeBChan commandChan $ C.CmdUpdateLog l e)

  -- Create a temporary chat
  _ <- store.swNewChat "#Temp" (Cfg.defaultModel cfg) C.SsNotStreaming

  void . forkIO . catchEx store "commands channel" $ E.runCommands commandChan eventChan store
  void . forkIO . catchEx store "events channel" $ E.runTick eventChan

  attrMap <- readAttrMap

  let app = B.App {
      B.appDraw = D.drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = E.handleEvent commandChan
    , B.appAttrMap = C._stAttrMap
    , B.appStartEvent = liftIO $ do
       BCh.writeBChan commandChan C.CmdRefreshModelList
       BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Left <$> cfg.acDefaultChatName)
       BCh.writeBChan commandChan $ C.CmdUpdateLog C.LlDebug "Starting TUI"
    }

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty

  now <- DT.getCurrentTime


  let initialState = C.UiState
       { _stTick = 0
       , _stAppConfig = cfg
       , _stTime = now
       , _stTab = C.TabModels
       , _stNow = now
       , _stDebug = ""
       , _stStore = store
       , _stLog = store.swLog
       , _stAttrMap = attrMap
       , _stErrorMessage = Nothing

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

       , _stColoursList = BL.list C.NColoursList (V.fromList . sort $ fst <$> U.knownColours) 1

       , _stFocusLog = BF.focusRing [C.NLogList]
       , _stLogList = BL.list C.NLogList mempty 1
       , _stLogFilterIndex = 1

       , _stPopup = Nothing

       , _stPopChatEditFocus = BF.focusRing [C.NPopChatEditName, C.NPopChatEditModels, C.NDialogOk, C.NDialogCancel]
       , _stPopChatEditName = BE.editorText C.NPopChatEditName (Just 1) ""
       , _stPopChatEditTitle = Nothing
       , _stPopChatEditModels = BL.list C.NPopChatEditModels mempty 1
       , _stPopChatEditOnOk = (\_ _ -> pass)

       , _stPopPromptEdit = BE.editorText C.NPopPromptEdit (Just 1) ""
       , _stPopPromptTitle = Nothing
       , _stPopPromptFocus = BF.focusRing [C.NPopPromptEdit, C.NDialogOk, C.NDialogCancel]
       , _stPopPromptOnOk = const pass

       , _stPopConfirmFocus = BF.focusRing [C.NDialogOk, C.NDialogCancel]
       , _stPopConfirmTitle = Nothing
       , _stPopConfirmOnOk = pass
       }

  _finalState <- B.customMain @C.Name initialVty buildVty (Just eventChan) app initialState
  pass

  where
    readAttrMap :: IO BA.AttrMap
    readAttrMap = do
      (_es, m) <- Cfg.loadTheme
      print _es
      pure m


    catchEx :: C.StoreWrapper -> Text -> IO () -> IO ()
    catchEx store n action = do
      catch
        action
        (\(e :: SomeException) -> do
          store.swLog.lgError $ "Exception: in " <> n <> "\n" <> show e
        )
