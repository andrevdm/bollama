{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Events
  ( handleEvent
  , runCommands
  , runTick
  ) where

import Verset
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick.Main qualified as BM
import Brick qualified as B
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Control.Exception.Safe (catch)
import Control.Lens ((.=), use)
import Data.Time qualified as DT
import Graphics.Vty qualified as Vty

import Config qualified as Cfg
import Core qualified as C
import Widgets.ErrorMessage qualified as Wer
import Widgets.PopupConfirm qualified as WPcm
import Widgets.PopupContextMenu qualified as WPctx
import Widgets.PopupEditChat qualified as WPce
import Widgets.PopupExport qualified as WPex
import Widgets.PopupHelp qualified as WPhlp
import Widgets.PopupPrompt qualified as WPpt
import Widgets.TabChat qualified as WTct
import Widgets.TabColours qualified as WTcl
import Widgets.TabLog qualified as WTlg
import Widgets.TabModels qualified as WTmo
import Widgets.TabPs qualified as WTps

---------------------------------------------------------------------------------------------------
-- Main Event Handler
---------------------------------------------------------------------------------------------------
handleEvent :: BCh.BChan C.Command -> BCh.BChan C.UiEvent -> B.BrickEvent C.Name C.UiEvent -> B.EventM C.Name C.UiState ()
handleEvent commandChan eventChan ev = do
  catch
    (do
      case ev of
        -- App events are global and must always be handled
        B.AppEvent ae -> handleAppEvent commandChan ae

        -- Mouse click
        B.MouseDown n b m l -> handleButtonDown n b m l

        -- Decide which handler to use
        B.VtyEvent ve -> do
          use C.stErrorMessage >>= \case
            -- Error message view gets priority
            Just _ -> do
              Wer.handleEventErrorMessage commandChan ev ve
            Nothing -> do
              use C.stPopup >>= \case
                -- Then the popup
                Just C.PopupChatEdit -> WPce.handleEventPopupChatEdit commandChan ev ve
                Just C.PopupPrompt -> WPpt.handleEventPopupPrompt commandChan ev ve
                Just C.PopupConfirm -> WPcm.handleEventPopupConfirm commandChan ev ve
                Just C.PopupHelp -> WPhlp.handleEventPopupHelp commandChan ev ve
                Just C.PopupContext -> WPctx.handleEventPopupContext commandChan ev ve
                Just C.PopupExport -> WPex.handleEventPopupExport commandChan ev ve
                -- Otherwise the main UI gets the event
                Nothing -> handleEventNoPopup commandChan eventChan ev ve

        _ -> pass
    )
    (\(e :: SomeException) -> do
      st <- B.get
      liftIO $ st._stLog.lgCritical $ "Exception in event handler: " <> show e
    )


handleButtonDown :: C.Name -> Vty.Button -> [Vty.Modifier] -> B.Location -> B.EventM C.Name C.UiState ()
handleButtonDown name button ms loc =
  case (name, button, ms) of
    (C.VScrollClick _ C.NChatScroll, _, _) -> WTct.handleButtonDown name button ms loc
    (C.NChatMsgCopy _, _, _) -> WTct.handleButtonDown name button ms loc
    _ -> pass



handleEventNoPopup :: BCh.BChan C.Command -> BCh.BChan C.UiEvent -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventNoPopup commandChan eventChan ev ve = do
  case ve of
    Vty.EvKey k ms -> do
      st <- B.get
      let
        focused =
          case st._stTab of
            C.TabModels -> BF.focusGetCurrent st._stFocusModels
            C.TabPs -> BF.focusGetCurrent st._stFocusPs
            C.TabChat -> BF.focusGetCurrent st._stFocusChat
            C.TabColours -> Nothing
            C.TabLog -> BF.focusGetCurrent st._stFocusLog


      case (st._stTab, focused, k, ms) of
        ---------------------------------------------------------------------------------------------------
        -- Global
        ---------------------------------------------------------------------------------------------------
        (_, _, Vty.KChar 'q', [Vty.MCtrl]) -> B.halt

        (_, _, Vty.KChar 'u', [Vty.MCtrl]) -> do
          (_es, m) <- liftIO $ Cfg.loadTheme
          C.stAttrMap .= m
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- Function keys
        ---------------------------------------------------------------------------------------------------
        (_, _, Vty.KFun 1, []) -> do
          when (isNothing st._stPopup) $
            C.stPopup .= Just C.PopupHelp

        (_, _, Vty.KFun 2, []) -> do
          unless (st._stTab == C.TabModels) $ do
            C.stLoadingPs .= True
            C.stTab .= C.TabModels

        (_, _, Vty.KFun 3, []) -> do
          unless (st._stTab == C.TabPs) $ do
            liftIO . BCh.writeBChan commandChan $ C.CmdRefreshPs
            C.stLoadingPs .= True
            C.stTab .= C.TabPs

        (_, _, Vty.KFun 4, []) -> do
          unless (st._stTab == C.TabChat) $ do
            C.stLoadingPs .= True
            C.stTab .= C.TabChat

        (_, _, Vty.KFun 11, []) -> do
            C.stTab .= C.TabColours

        (_, _, Vty.KFun 12, []) -> do
            C.stTab .= C.TabLog
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- Tabs
        ---------------------------------------------------------------------------------------------------
        (C.TabModels, _, _, _) -> WTmo.handleTabModels commandChan ev ve focused k ms
        (C.TabPs, _, _, _) -> WTps.handleTabPs commandChan ev ve focused k ms
        (C.TabChat, _, _, _) -> WTct.handleTabChat commandChan eventChan st._stStore ev ve focused k ms
        (C.TabColours, _, _, _) -> WTcl.handleTabColours commandChan ev ve focused k ms
        (C.TabLog, _, _, _) -> WTlg.handleTabLog commandChan ev ve focused k ms
        ---------------------------------------------------------------------------------------------------

    _ -> pass
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- App Events
---------------------------------------------------------------------------------------------------
handleAppEvent :: BCh.BChan C.Command -> C.UiEvent -> B.EventM C.Name C.UiState ()
handleAppEvent commandChan uev = do
  case uev of
    C.UeInit -> handleInit
    C.UeTick t -> handleAppEventTick commandChan t
    C.UeGotModelList ms1 -> WTmo.handleAppEventGotModelList commandChan ms1
    C.UePsList ps' -> WTps.handleAppEventPsList commandChan ps'
    C.UeGotModelShow x -> WTmo.handleAppEventGotModelShow commandChan x
    C.UeModelShowDone -> WTmo.handleAppEventModelShowDone commandChan
    C.UeGotTime t -> handleAppEventGotTime commandChan t
    C.UeChatUpdated chatId -> WTct.handleChatUpdated chatId
    C.UeChatStreamResponseDone chatId detail -> WTct.handleChatStreamResponseDone commandChan chatId detail
    C.UeGotChatsList chats overrideSelect' -> WTct.gotChatsList chats overrideSelect'
    C.UeLogUpdated ls (lvl, msg) -> WTlg.logUpdated ls (lvl, msg)


handleInit :: B.EventM C.Name C.UiState ()
handleInit = do
  cfg <- use C.stAppConfig
  when cfg.acAllowMouse $ do
    vty <- BM.getVtyHandle
    let output = vty.outputIface
    when (Vty.supportsMode output Vty.Mouse) $
      liftIO $ Vty.setMode output Vty.Mouse True


handleAppEventTick :: BCh.BChan C.Command -> Int -> B.EventM C.Name C.UiState ()
handleAppEventTick commandChan t = do
  C.stTick .= t
  currentTab <- use C.stTab

  when (t `mod` 50 == 0 && currentTab == C.TabPs) $ do
    liftIO (BCh.writeBChan commandChan C.CmdRefreshPs)


handleAppEventGotTime :: BCh.BChan C.Command -> DT.UTCTime -> B.EventM C.Name C.UiState ()
handleAppEventGotTime _commandChan t = do
  C.stTime .= t
---------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- background thread
----------------------------------------------------------------------------------------------------------------------
runCommands :: TV.TVar C.RunState -> C.AppConfig -> BCh.BChan C.Command -> BCh.BChan C.UiEvent -> C.StoreWrapper -> IO ()
runCommands rs' cfg commandChan eventChan store = forever $ do
  BCh.readBChan commandChan >>= \case
    C.CmdRefreshModelList -> WTmo.refreshModelsList eventChan
    C.CmdRefreshModelShow names -> WTmo.refreshModelsShow cfg names eventChan
    C.CmdRefreshPs -> WTps.refreshPs eventChan cfg
    C.CmdChatSend chatId msg -> WTct.chatSend rs' cfg eventChan store chatId msg
    C.CmdRefreshChatsList overrideSelect1 -> WTct.refreshChatsList eventChan store overrideSelect1
    C.CmdUpdateLog l e -> WTlg.updateLog eventChan store l e


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
