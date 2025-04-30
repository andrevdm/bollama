{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module App where

import           Verset

import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
--import Brick.Widgets.Border qualified as BB
--import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Data.Time qualified as DT
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Core qualified as C
import Config qualified as Cfg
import Draw qualified as D
import Events qualified as E



runTui :: IO ()
runTui = do
  eventChan <- BCh.newBChan 50

  commandChan <- BCh.newBChan @C.Command 50
  void . forkIO $ runCommands commandChan eventChan
  void . forkIO $ runTick eventChan

  let app = B.App {
      B.appDraw = D.drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = E.handleEvent commandChan
    , B.appAttrMap = const D.attrMap
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

       , _stFocusChat = BF.focusRing []
       }

  _finalState <- B.customMain @C.Name initialVty buildVty (Just eventChan) app initialState
  pass




----------------------------------------------------------------------------------------------------------------------
-- background thread
----------------------------------------------------------------------------------------------------------------------
runCommands :: BCh.BChan C.Command -> BCh.BChan C.UiEvent -> IO ()
runCommands commandChan eventChan = forever $ do
  BCh.readBChan commandChan >>= \case
    C.CmdRefreshModelList -> do
      mis' <- O.list
      let ms = maybe [] (\(O.Models x) -> x) mis'
      BCh.writeBChan eventChan . C.UeGotModelList $ ms


    C.CmdRefreshModelShow names -> do
      forConcurrently_ names $ \n -> do
        s' <- O.showModelOps (Just C.ollamaUrl) n Nothing
        case s' of
          Nothing -> pass
          Just s -> (BCh.writeBChan eventChan $ C.UeGotModelShow (n, s))

      BCh.writeBChan eventChan $ C.UeModelShowDone


    C.CmdRefreshPs -> do
      ps' <- O.psOps (Just C.ollamaUrl)
      let ps = maybe [] (\(O.RunningModels x) -> x) ps'
      BCh.writeBChan eventChan . C.UePsList $ ps



runTick :: BCh.BChan C.UiEvent -> IO ()
runTick eventChan = do
  tick' <- TV.newTVarIO (0 :: Int)

  forever $ do
    tick <- atomically . TV.stateTVar tick' $ \t ->
      let t2 = t + 1 `mod` 1_000_000 in
      (t2, t2)

    BCh.writeBChan eventChan $ C.UeTick tick

    when (tick `mod` 10 == 0) $ do
      now <- DT.getCurrentTime
      BCh.writeBChan eventChan $ C.UeGotTime now

    threadDelay 100_000
----------------------------------------------------------------------------------------------------------------------
