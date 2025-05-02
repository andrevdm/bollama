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
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Graphics.Vty.CrossPlatform qualified as Vty

import Core qualified as C
import Config qualified as Cfg
import Draw qualified as D
import Events qualified as E
import Storage qualified as Sr
import Utils qualified as U


runTui :: IO ()
runTui = do
  store <- Sr.newStoreWrapper Sr.newInMemStore

  eventChan <- BCh.newBChan 1000
  commandChan <- BCh.newBChan @C.Command 1000

  void . forkIO $ E.runCommands commandChan eventChan store
  void . forkIO $ E.runTick eventChan

  attrMap <- readAttrMap

  let app = B.App {
      B.appDraw = D.drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = E.handleEvent commandChan
    , B.appAttrMap = C._stAttrMap
    , B.appStartEvent = liftIO $ do
       BCh.writeBChan commandChan C.CmdRefreshModelList
    }

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty

  now <- DT.getCurrentTime
  cfg <- Cfg.loadAppConfig


  let initialState = C.UiState
       { _stTick = 0
       , _stAppConfig = cfg
       , _stTime = now
       , _stTab = C.TabModels
       , _stNow = now
       , _stDebug = ""
       , _stStore = store
       , _stAttrMap = attrMap
       , _stFooterWidget = Nothing

       , _stModels = []
       , _stModelsList = BL.list C.NModelsList mempty 1
       , _stModelsFilter = ""
       , _stModelListLoading = True
       , _stModelShowLoading = True
       , _stFocusModels = BF.focusRing [C.NModelsList]
       , _stModelFilterEditor = BE.editorText C.NModelEditSearch (Just 1) ""
       , _stModelTagEditor = BE.editorText C.NModelEditTag (Just 1) ""

       , _stPs = BL.list C.NListPs mempty 1
       , _stFocusPs = BF.focusRing [C.NListPs]
       , _stLoadingPs = True

       , _stFocusChat = BF.focusRing [C.NChatInputEdit, C.NChatMsgList]
       , _stChatInput = BE.editorText C.NChatInputEdit (Just 5) ""
       , _stChatCurrent = Nothing
       , _stChatMsgList = BL.list C.NChatMsgList mempty 1

       , _stColoursList = BL.list C.NColoursList (V.fromList . sort $ fst <$> U.knownColours) 1
       }

  _finalState <- B.customMain @C.Name initialVty buildVty (Just eventChan) app initialState
  pass

  where
    readAttrMap :: IO BA.AttrMap
    readAttrMap = do
      (_es, m) <- U.attrMapFromFile "defaultAttrs.csv"
      print _es
      pure m
