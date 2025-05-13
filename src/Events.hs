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

import           Verset

import Brick qualified as B
import Brick.Main qualified as BM
import Brick.BChan qualified as BCh
import Brick.Forms qualified as BFm
import Brick.Focus qualified as BF
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.FileBrowser qualified as BFi
import Brick.Widgets.List qualified as BL
import Control.Debounce as Deb
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Control.Exception.Safe (catch)
import Control.Lens ((^.), (^?), (%=), (.=), use, to, at)
import Data.Aeson qualified as Ae
import Data.List (findIndex)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O
import System.Directory qualified as Dir
import System.FilePath qualified as Fp
import System.FilePath ((</>))
import System.Hclip qualified as Clip

import Config qualified as Cfg
import Core qualified as C
import Draw qualified as D
import Utils qualified as U


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
              handleEventErrorMessage commandChan ev ve
            Nothing -> do
              use C.stPopup >>= \case
                -- Then the popup
                Just C.PopupChatEdit -> handleEventPopupChatEdit commandChan ev ve
                Just C.PopupPrompt -> handleEventPopupPrompt commandChan ev ve
                Just C.PopupConfirm -> handleEventPopupConfirm commandChan ev ve
                Just C.PopupHelp -> handleEventPopupHelp commandChan ev ve
                Just C.PopupContext -> handleEventPopupContext commandChan ev ve
                Just C.PopupExport -> handleEventPopupExport commandChan ev ve
                -- Otherwise the main UI gets the event
                Nothing -> handleEventNoPopup commandChan eventChan ev ve

        _ -> pass
    )
    (\(e :: SomeException) -> do
      st <- B.get
      liftIO $ st._stLog.lgCritical $ "Exception in event handler: " <> show e
    )


handleButtonDown :: C.Name -> Vty.Button -> [Vty.Modifier] -> B.Location -> B.EventM C.Name C.UiState ()
handleButtonDown name button ms _loc =
  case (name, button, ms) of
    -- Handle mouse events for the scrollbar
    (C.VScrollClick se C.NChatScroll, _, _) -> do
      case  se of
        B.SBHandleBefore -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up
        B.SBHandleAfter -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down
        B.SBTroughBefore -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up
        B.SBTroughAfter -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down
        _ -> pass

    (C.NChatMsgCopy msgId, _, _) -> do
      store <- use C.stStore
      liftIO (store.swGetMessageText msgId) >>= \case
        Nothing -> pass
        Just msg -> do
          C.stDebug .= "Copying message to clipboard"
          liftIO . Clip.setClipboard . Txt.unpack $ msg

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
        (C.TabModels, _, _, _) -> handleTabModels commandChan ev ve focused k ms
        (C.TabPs, _, _, _) -> handleTabPs commandChan ev ve focused k ms
        (C.TabChat, _, _, _) -> handleTabChat commandChan eventChan st._stStore ev ve focused k ms
        (C.TabColours, _, _, _) -> handleTabColours commandChan ev ve focused k ms
        (C.TabLog, _, _, _) -> handleTabLog commandChan ev ve focused k ms
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
    C.UeGotModelList ms1 -> handleAppEventGotModelList commandChan ms1
    C.UePsList ps' -> handleAppEventPsList commandChan ps'
    C.UeGotModelShow x -> handleAppEventGotModelShow commandChan x
    C.UeModelShowDone -> handleAppEventModelShowDone commandChan
    C.UeGotTime t -> handleAppEventGotTime commandChan t
    C.UeChatUpdated chatId -> handleChatUpdated chatId
    C.UeChatStreamResponseDone chatId detail -> handleChatStreamResponseDone commandChan chatId detail

    C.UeGotChatsList chats overrideSelect' -> do
      C.stChatsList %= BL.listReplace (V.fromList chats) Nothing

      case overrideSelect' of
        Just chatId -> C.stChatsList %= BL.listFindBy (\i -> i.chatId == chatId)
        Nothing -> C.stChatsList %= BL.listMoveTo 0

      handleChatSelectionUpdate

    C.UeLogUpdated ls (lvl, msg) -> do
      C.stLogList %= BL.listReplace (V.fromList ls) Nothing
      C.stLogList %= BL.listMoveToEnd

      when (lvl `elem` [C.LlCritical, C.LlError, C.LlWarn]) $ do
        C.stDebug .= (U.logLevelName lvl) <> ":" <> msg

      when (lvl `elem` [C.LlCritical, C.LlError]) $ do
        C.stErrorMessage .= Just msg


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


handleAppEventGotModelList :: BCh.BChan C.Command -> [O.ModelInfo] -> B.EventM C.Name C.UiState ()
handleAppEventGotModelList commandChan ms1 = do
  cfg <- use C.stAppConfig
  prevList <- use C.stModelsList

  let ms2 = ms1
  let ms = ms2 <&> \m -> C.ModelItem
        { miName = m.name
        , miInfo = m
        , miShow = Nothing
        , miTag = fromMaybe "" $ cfg.acModelTag ^. at m.name
        }
      prevSelected = snd <$> BL.listSelectedElement prevList
      prevSelectedName = (.miName) <$> prevSelected

  C.stModels .= ms
  filteredModels <- filterModels
  let ix = findIndex (\x -> Just x.miName == prevSelectedName) filteredModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix
  C.stModelListLoading .= False

  C.stModelShowLoading .= True
  liftIO . BCh.writeBChan commandChan $ C.CmdRefreshModelShow (ms <&> C.miName)


handleAppEventPsList :: BCh.BChan C.Command -> [O.RunningModel] -> B.EventM C.Name C.UiState ()
handleAppEventPsList _commandChan ps' = do
  let ps = sortOn (.modelName) ps'
  psl <- use C.stPs
  let wasSelected = fromMaybe "" $ (.modelName) . snd <$> BL.listSelectedElement psl
  C.stPs .= BL.listFindBy (\i -> i.modelName == wasSelected) (BL.list C.NModelsList (V.fromList ps) 1)
  C.stLoadingPs .= False


handleAppEventGotModelShow :: BCh.BChan C.Command -> (Text, O.ShowModelResponse) -> B.EventM C.Name C.UiState ()
handleAppEventGotModelShow _commandChan (m, s) = do
  l1 <- use C.stModelsList
  vs1 <- use C.stModels

  let vs2 = vs1 <&> \old ->
       if old.miName == m
         then old { C.miShow = Just s }
         else old

      selected = snd <$> BL.listSelectedElement l1
      selectedName = (.miName) <$> selected

  C.stModels .= vs2
  filteredModels <- filterModels

  let ix = findIndex (\x -> Just x.miName == selectedName) filteredModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix


handleAppEventModelShowDone :: BCh.BChan C.Command -> B.EventM C.Name C.UiState ()
handleAppEventModelShowDone _commandChan = do
  l1 <- use C.stModelsList
  filteredModels <- filterModels

  let
    selected = snd <$> BL.listSelectedElement l1
    selectedName = (.miName) <$> selected
    ix = findIndex (\x -> Just x.miName == selectedName) filteredModels

  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix
  C.stModelShowLoading .= False


handleAppEventGotTime :: BCh.BChan C.Command -> DT.UTCTime -> B.EventM C.Name C.UiState ()
handleAppEventGotTime _commandChan t = do
  C.stTime .= t


handleChatStreamResponseDone :: BCh.BChan C.Command -> C.ChatId -> Maybe C.MessageDetail -> B.EventM C.Name C.UiState ()
handleChatStreamResponseDone _commandChan chatId detail = do
  -- Save to store
  store <- use C.stStore
  liftIO $ store.swStreamDone chatId detail

  -- Update with the final response
  --  Some updates may have been suppressed by the debounce
  --  This also updates the stCurrent's streaming status
  handleChatUpdated chatId


handleChatUpdated :: C.ChatId -> B.EventM C.Name C.UiState ()
handleChatUpdated chatId  = do
  use C.stChatCurrent >>= \case
    Just currentChat | currentChat.chatId == chatId -> do
      changeChatToStoreCurrent

    _ -> do
      pass
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Models Tab Events
---------------------------------------------------------------------------------------------------
handleTabModels
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabModels commandChan _ev ve focused k ms =
  case (focused, k, ms) of
    (Just C.NModelsList, Vty.KChar '/', []) -> do
      currentFilter <- use C.stModelsFilter
      C.stPopup .= Just C.PopupPrompt
      C.stPopPromptEdit . BE.editContentsL .= TxtZ.textZipper [currentFilter] Nothing
      C.stPopPromptTitle .= Just "Model filter"
      C.stPopPromptOnOk .= \txt -> do
        C.stModelsFilter .= txt
        filteredModels <- filterModels
        wasSelected <- B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName)
        let ix = findIndex (\x -> Just x.miName == wasSelected) filteredModels
        C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix

    (Just C.NModelsList, Vty.KChar 'c', []) -> do
      tags <- do
        cfg <- use C.stAppConfig
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
          Nothing -> pure ""
          Just selected -> pure . maybe "" (Txt.strip . Txt.replace "\n" " " . Txt.replace "\r" " ") $ Map.lookup selected cfg.acModelTag

      C.stPopup .= Just C.PopupPrompt
      C.stPopPromptEdit . BE.editContentsL .= TxtZ.textZipper [tags] Nothing
      C.stPopPromptTitle .= Just "User comment edit"
      C.stPopPromptOnOk .= \txt -> do
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
          Nothing -> pass
          Just selected -> do
            C.stAppConfig %= \cfg2 -> cfg2 { C.acModelTag = Map.insert selected txt cfg2.acModelTag }
            liftIO . Cfg.writeAppConfig =<< use C.stAppConfig

    (Just C.NModelsList, Vty.KChar '*', []) -> do
      B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
        Nothing -> pass
        Just selected -> do
          C.stAppConfig %= \cfg2 -> cfg2 { C.acDefaultModel = Just selected }
          liftIO . Cfg.writeAppConfig =<< use C.stAppConfig
          C.stDebug .= "Default model set to: " <> selected

    (Just C.NModelsList, Vty.KChar 'd', []) -> do
      B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
        Nothing -> pass
        Just selected -> do
          C.stPopup .= Just C.PopupConfirm
          C.stPopConfirmTitle .= Just "Are you sure you want to remove this model?"
          C.stPopConfirmDetail .= Just ("   " <> selected)
          C.stPopConfirmOnOk .= do
            catch
              (do
                 st <- B.get
                 liftIO $ O.deleteModelOps (Just $ fromMaybe C.ollamaDefaultUrl st._stAppConfig.acOllamaUrl) selected
                 liftIO $ BCh.writeBChan commandChan C.CmdRefreshModelList
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting model: " <> show e
              )

    -- Use current model for #Temp chat
    (Just C.NModelsList, Vty.KChar 't', []) -> do
      chats <- use C.stChatsList
      case find (\i -> i.chatName == "#Temp") chats of
        Nothing -> C.stDebug .= "No temp chat" --TODO
        Just chat1 -> do
          B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
            Nothing -> C.stDebug .= "No model selected" --TODO
            Just model -> do
              store <- use C.stStore
              now <- liftIO DT.getCurrentTime

              let chat2 = chat1
                    { C.chatModel = model
                    , C.chatUpdatedAt = now
                    }

              C.stTab .= C.TabChat
              C.stFocusChat %= BF.focusSetCurrent C.NChatInputEdit
              liftIO $ store.swSaveChat chat2
              liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat2.chatId) --TODO dont send message just call change chat


    -- Use current model for a new chat
    (Just C.NModelsList, Vty.KChar 'n', []) -> do
      C.stTab .= C.TabChat
      modelName <- B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName)
      startNewChat commandChan modelName


    (Just C.NModelsList, Vty.KFun 5, []) -> do
      liftIO $ BCh.writeBChan commandChan C.CmdRefreshModelList

    (Just C.NModelsList, _, _) -> do
      B.zoom C.stModelsList $ BL.handleListEventVi BL.handleListEvent ve

    _ -> pass
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Ps Tab Events
---------------------------------------------------------------------------------------------------
handleTabPs
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabPs _commandChan _ev ve focused k ms = do
  case (focused, k, ms) of
    (Just C.NListPs, Vty.KChar 's', []) -> do
      B.gets (^?  C.stPs . BL.listSelectedElementL . to (.modelName)) >>= \case
        Nothing -> pass
        Just name -> do
          st <- B.get
          _ <- liftIO . O.generate $ O.GenerateOps
            { modelName = name
            , keepAlive = Just "0"
            , prompt = ""
            , suffix = Nothing
            , images = Nothing
            , format = Nothing
            , system = Nothing
            , template = Nothing
            , stream = Nothing
            , raw = Nothing
            , hostUrl = Just $ fromMaybe C.ollamaDefaultUrl st._stAppConfig.acOllamaUrl
            , responseTimeOut = Nothing
            , options = Nothing
            }
          C.stDebug .= "stopping " <> name

      pass

    (Just C.NListPs, _, _) -> do
      B.zoom C.stPs $ BL.handleListEventVi BL.handleListEvent ve

    _ -> pass
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Chat Tab Events
---------------------------------------------------------------------------------------------------
handleTabChat
  :: BCh.BChan C.Command
  -> BCh.BChan C.UiEvent
  -> C.StoreWrapper
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabChat commandChan eventChan store ev ve focused k ms =
  case (focused, k, ms) of
    (_, Vty.KChar '\t', []) -> do
      C.stFocusChat %= BF.focusNext

    (_, Vty.KBackTab, []) -> do
      C.stFocusChat %= BF.focusPrev

    (_, Vty.KChar 'n', [Vty.MCtrl]) -> do
      startNewChat commandChan Nothing

    (_, Vty.KChar 'e', [Vty.MCtrl]) -> do
        editModel "Edit chat"

    (_, Vty.KFun 10, []) -> do
      contextMenu

    (_, Vty.KPageUp, []) -> do
      B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up

    (_, Vty.KPageUp, [Vty.MCtrl]) -> do
      B.vScrollToBeginning (B.viewportScroll C.NChatScroll)

    (_, Vty.KPageDown, []) -> do
      B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down

    (_, Vty.KPageDown, [Vty.MCtrl]) -> do
      B.vScrollToEnd (B.viewportScroll C.NChatScroll)

    (_, Vty.KChar 't', [Vty.MCtrl]) -> do
      C.stShowThinking %= not

    (_, Vty.KChar 'p', [Vty.MCtrl]) -> do
      C.stShowMessageDetail %= not

      -- Scroll to the end if the detail is not shown because content was removed and shifted up
      use C.stShowMessageDetail >>= \case
        True -> pass
        False -> B.vScrollToEnd (B.viewportScroll C.NChatScroll)

    (_, Vty.KChar 'c', [Vty.MCtrl]) -> do
      stopChat

    (Just C.NChatInputEdit, Vty.KChar 's', [Vty.MCtrl]) -> do
      runInput

    (Just C.NChatInputEdit, Vty.KFun 5, []) -> do
      runInput

    (Just C.NChatInputEdit, _, _) -> do
      B.zoom C.stChatInput $ BE.handleEditorEvent ev

    (Just C.NChatsList, Vty.KChar '*', _) -> do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
        Nothing -> pass
        Just (_, chat) -> do
          C.stDebug .= "Default chat set: " <> chat.chatName
          C.stAppConfig %= \cfg2 -> cfg2 { C.acDefaultChat = Just (C.unChatId chat.chatId) }
          liftIO . Cfg.writeAppConfig =<< use C.stAppConfig


    (Just C.NChatsList, _, _) -> do
      B.zoom C.stChatsList $ BL.handleListEventVi BL.handleListEvent ve
      handleChatSelectionUpdate

    _ -> pass

  where
    contextMenu = do
      let menuItems =
           [ ("export", "Export chat")
           , ("edit", "Edit chat")
           , ("new", "New chat")
           , ("think", "Toggle thinking")
           , ("detail", "Toggle message detail")
           , ("stop", "Stop chat")
           , ("clear", "Delete messages")
           ]
      C.stPopup .= Just C.PopupContext
      C.stPopContextTitle .= Just "Chat"
      C.stPopContextList .= BL.list C.NPopContextList (V.fromList menuItems) 1
      C.stPopContextOnOk .= \case
        "export" -> exportChat
        "edit" -> editModel "Edit chat"
        "new" -> startNewChat commandChan Nothing
        "think" -> C.stShowThinking %= not
        "detail" -> C.stShowMessageDetail %= not
        "stop" -> stopChat
        "clear" -> clearChatMessages
        _ -> pass


    exportChat = do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
       Nothing -> C.stDebug .= "No chat selected"
       Just (_, chat1) -> do
          liftIO (store.swGetChat chat1.chatId) >>= \case
            Nothing -> C.stDebug .= "No chat found"
            Just (chat, ms') -> do
              C.stPopup .= Just C.PopupExport
              C.stPopExportOnOk .= \exportFormat path -> do
                liftIO $ U.exportChatToFile chat ms' exportFormat path
                C.stDebug .= "Exported chat to: " <> Txt.pack path


    clearChatMessages = do
      C.stPopup .= Just C.PopupConfirm
      C.stPopConfirmTitle .= Just "Are you sure you want to remove all messages for this chat?"
      C.stPopConfirmDetail .= Nothing
      C.stPopConfirmOnOk .= do
        use (C.stChatsList . to BL.listSelectedElement) >>= \case
          Nothing -> pass
          Just (_, chat) -> do
            catch
              (do
                 st <- B.get
                 C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                 liftIO $ st._stStore.swDeleteAllChatMessages chat.chatId
                 liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting chat messages: " <> show e
              )


    stopChat = do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
        Nothing -> pass
        Just (_, chat) -> do
          st <- B.get
          liftIO (st._stStore.swGetChat chat.chatId) >>= \case
            Nothing -> pass
            Just (_, ms1) -> do
              case reverse ms1 of
                [] -> pass
                (m:_) -> do
                  lg <- use C.stLog
                  rs' <- use C.stRunState
                  liftIO . lg.lgWarn $ "Stopping chat: " <> chat.chatName
                  liftIO . atomically . TV.modifyTVar' rs' $ \rs -> rs
                    { C.rsKilledChats = Set.insert m.msgId rs.rsKilledChats
                    }

                  liftIO . BCh.writeBChan eventChan $ C.UeChatStreamResponseDone chat.chatId Nothing

    editModel title = do
     use C.stChatCurrent >>= \case
       Nothing -> pass
       Just chat -> do
         st <- B.get

         C.stPopChatEditForm .= BFm.setFormConcat (D.vBoxWithPadding 1) (D.mkPopChatEditForm C.ChatEditInfo
           { C._ceiModels = st._stModels
           , C._ceiSelectedModel = find (\i -> i.miName == chat.chatModel) st._stModels
           , C._ceiName = chat.chatName
           , C._ceiParams = chat.chatParams
           })

         C.stPopup .= Just C.PopupChatEdit
         C.stPopChatEditTitle .= Just title
         C.stPopChatEditOnOk .= \name model prms -> do
           let chat2 = chat
                { C.chatName = name
                , C.chatModel = model.miName
                , C.chatParams = prms
                }
           liftIO $ store.swSaveChat chat2
           C.stChatCurrent .= Just chat2
           liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)


    runInput = do
      liftIO store.swGetCurrent >>= \case
        Just (cid, chat, _) -> do
          txt <- use (C.stChatInput . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

          unless (Txt.null txt) $ do
            findModel chat.chatModel >>= \case
              Just _model -> do
                liftIO (store.swAddMessage cid O.User C.SsStreaming chat.chatModel txt) >>= \case
                  Right newMsg -> do
                    C.stChatCurrent .= Just chat {C.chatStreaming = C.SsStreaming}
                    C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                    _ <- liftIO $ store.swClearChatInput cid
                    handleChatUpdated cid
                    liftIO . BCh.writeBChan commandChan $ C.CmdChatSend cid newMsg

                  Left err -> do
                    liftIO . store.swLog.lgError $ "Error sending message: " <> err

              Nothing -> do
                C.stDebug .= "Invalid model name: " <> chat.chatModel
                editModel "Select a model"


        Nothing -> do
          liftIO $ store.swLog.lgError "No current chat"


    findModel n = do
      models <- use C.stModels
      pure $ find (\m -> m.miName == n) models


startNewChat :: BCh.BChan C.Command -> Maybe Text -> B.EventM C.Name C.UiState ()
startNewChat commandChan defaultModel = do
  st <- B.get
  store <- use C.stStore
  C.stPopup .= Just C.PopupChatEdit
  C.stPopChatEditTitle .= Just "New chat"
  C.stPopChatEditForm .= BFm.setFormConcat (D.vBoxWithPadding 1) (D.mkPopChatEditForm C.emptyChatEditInfo
    { C._ceiModels = st._stModels
    , C._ceiSelectedModel = find (\i -> Just i.miName == defaultModel) st._stModels
    })
  C.stPopChatEditOnOk .= \name model prms -> do
    chat <- liftIO $ store.swNewChat C.SsNotStreaming name model.miName prms
    liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)



handleChatSelectionUpdate :: B.EventM C.Name C.UiState ()
handleChatSelectionUpdate = do
  saveStoreCurrentChat

  store <- use C.stStore
  selectedChat <- use (C.stChatsList . to BL.listSelectedElement)
  _ <- liftIO $ store.swSetCurrent ((C.chatId) . snd <$> selectedChat)

  changeChatToStoreCurrent


saveStoreCurrentChat :: B.EventM C.Name C.UiState ()
saveStoreCurrentChat = do
  store <- use C.stStore
  liftIO store.swGetCurrent >>= \case
    Nothing -> pass
    Just (_, chat1, _) -> do
      st <- B.get
      let chat2 = chat1
            { C.chatInput = st._stChatInput ^. BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip
            }
      liftIO $ store.swSaveChat chat2


changeChatToStoreCurrent :: B.EventM C.Name C.UiState ()
changeChatToStoreCurrent = do
  store <- use C.stStore

  liftIO store.swGetCurrent >>= \case
    Just (_, storeCurrentChat, ms) -> do
      C.stChatCurrent .= Just storeCurrentChat
      C.stChatInput . BE.editContentsL .= TxtZ.textZipper [storeCurrentChat.chatInput] Nothing
      C.stChatMsgs .= ms
      B.vScrollToEnd (B.viewportScroll C.NChatScroll)
      liftIO . Txt.appendFile "/home/andre/temp/a.txt" $ Txt.intercalate "\n" $ C.msgText <$> ms

    Nothing -> do
      C.stChatCurrent .= Nothing
      C.stChatInput . BE.editContentsL .= TxtZ.textZipper [] Nothing
      C.stChatMsgs .= []
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Colours Tab Events
---------------------------------------------------------------------------------------------------
handleTabColours
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabColours _commandChan _ev ve _focused _k _ms = do
  B.zoom C.stColoursList $ BL.handleListEventVi BL.handleListEvent ve
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Log Tab Events
---------------------------------------------------------------------------------------------------
handleTabLog
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabLog _commandChan _ev ve _focused k ms = do
  case (k, ms) of
    (Vty.KChar '+', []) -> do
      --let fs = length U.logLevelFilters
      pass

    (Vty.KChar '-', []) -> do
      --let fs = length U.logLevelFilters
      pass

    _ -> do
      --C.stDebug .= show (k, ms)
      B.zoom C.stLogList $ BL.handleListEventVi BL.handleListEvent ve
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------
filterModels :: B.EventM C.Name C.UiState [C.ModelItem]
filterModels = do
  vs <- use C.stModels
  t <- Txt.strip . Txt.toLower <$> use C.stModelsFilter
  let vs2 =
       if Txt.null t
        then vs
        else
          filter (\mi ->
            let
              name = Txt.strip . Txt.toLower $ mi.miName
              capabilities = Txt.toLower . Txt.intercalate " " . fromMaybe [] . join $ mi.miShow <&> (.capabilities)
              tags = Txt.toLower mi.miTag
            in
            Txt.isInfixOf t (name <> " " <> capabilities <> " " <> tags)
          )
          vs
  pure . reverse $ sortOn U.parseParams vs2
---------------------------------------------------------------------------------------------------




----------------------------------------------------------------------------------------------------------------------
-- background thread
----------------------------------------------------------------------------------------------------------------------
runCommands :: TV.TVar C.RunState -> C.AppConfig -> BCh.BChan C.Command -> BCh.BChan C.UiEvent -> C.StoreWrapper -> IO ()
runCommands rs' cfg commandChan eventChan store = forever $ do
  BCh.readBChan commandChan >>= \case
    C.CmdRefreshModelList -> do
      mis' <- O.list
      let ms = maybe [] (\(O.Models x) -> x) mis'
      BCh.writeBChan eventChan . C.UeGotModelList $ ms


    C.CmdRefreshModelShow names -> do
      forConcurrently_ names $ \n -> do
        s' <- O.showModelOps (Just $ fromMaybe C.ollamaDefaultUrl cfg.acOllamaUrl) n Nothing
        case s' of
          Nothing -> pass
          Just s -> (BCh.writeBChan eventChan $ C.UeGotModelShow (n, s))

      BCh.writeBChan eventChan $ C.UeModelShowDone


    C.CmdRefreshPs -> do
      ps' <- O.psOps (Just $ fromMaybe C.ollamaDefaultUrl cfg.acOllamaUrl)
      let ps = maybe [] (\(O.RunningModels x) -> x) ps'
      BCh.writeBChan eventChan . C.UePsList $ ps


    C.CmdChatSend chatId msg -> do
      store.swGetChat chatId >>= \case
        Just (chat, hist1) -> do
          debouncedUpdateUi <- Deb.mkDebounce Deb.defaultDebounceSettings
            { Deb.debounceFreq = 500_000  -- 500ms
            , Deb.debounceEdge = Deb.leadingEdge
            , Deb.debounceAction = BCh.writeBChan eventChan $ C.UeChatUpdated chatId
            }

          streamingMessageId <- liftIO $ C.MessageId <$> U.newUuidText

          let
            hist2 = hist1 <&> \m -> O.Message m.msgRole m.msgText Nothing Nothing
            oMsg = O.Message msg.msgRole msg.msgText Nothing Nothing
            msgAndCtx = NE.fromList $ hist2 <> [oMsg]

            params = catMaybes
              [ case chat.chatParams._cpTemp of
                  Nothing -> Nothing
                  Just t -> Just ("temperature", Ae.Number . realToFrac $ t)
              , case chat.chatParams._cpContextSize of
                  Nothing -> Nothing
                  Just t -> Just ("num_ctx", Ae.Number . fromIntegral $ t)
              ]

            ops = O.ChatOps
              { chatModelName = chat.chatModel
              , messages = msgAndCtx
              , tools = Nothing
              , format = Nothing
              , keepAlive = Just "5m"
              , hostUrl = Just $ fromMaybe C.ollamaDefaultUrl cfg.acOllamaUrl
              , responseTimeOut = Nothing
              , options = Just . Ae.object $ params
              , stream = Just (
                \cr -> do
                  rs <- TV.readTVarIO rs'
                  unless (Set.member streamingMessageId rs.rsKilledChats) $ do
                    case cr.message of
                      Nothing -> pass
                      Just m -> do
                        _ <- liftIO $ store.swAddStreamedChatContent chatId streamingMessageId m.role m.content
                        debouncedUpdateUi

                    when (cr.done) $ do
                      BCh.writeBChan eventChan $ C.UeChatStreamResponseDone chatId (Just $ U.messageDetailFromChatResponse cr)
                      atomically $ TV.modifyTVar' rs' $ \rs2 -> rs2 { C.rsKilledChats = Set.delete streamingMessageId rs2.rsKilledChats }
                , pure   ()
                )
              }

          void . forkIO $ do
            catch
              (do
                _r <- O.chat ops
                --TODO check response
                pass
              )
              (\(e :: SomeException) -> do
                store.swLog.lgError $ "Exception in chat stream: \n" <> show e
              )

        Nothing -> do
          --TODO send message for error
          pass

    C.CmdRefreshChatsList overrideSelect1 -> do
      chats1 <- store.swListChats
      let chats = reverse $ sortOn (C.chatUpdatedAt) chats1
      let overrideSelect =
           case overrideSelect1 of
             Nothing -> Nothing
             Just (Right chatId) -> Just chatId
             Just (Left chatIdent) -> C.chatId <$> find (\c -> (C.unChatId c.chatId) == chatIdent || c.chatName == chatIdent) chats

      BCh.writeBChan eventChan $ C.UeGotChatsList chats overrideSelect


    C.CmdUpdateLog l e -> do
      ls <- store.swLog.lgReadLast 1000
      BCh.writeBChan eventChan $ C.UeLogUpdated ls (l, e)




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



----------------------------------------------------------------------------------------------------------------------
-- Error Message
----------------------------------------------------------------------------------------------------------------------
handleEventErrorMessage :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventErrorMessage _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopPromptFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stErrorMessage .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stErrorMessage .= Nothing

        (_, Vty.KEnter, []) -> do
          C.stErrorMessage .= Nothing

        _ -> pass

      pass
    _ -> pass
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Edit Chat Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupChatEdit :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupChatEdit _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopChatEditFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          clear

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopChatEditFocus %= BF.focusNext
          updateFocus

        (_, Vty.KBackTab, []) -> do
          C.stPopChatEditFocus %= BF.focusPrev
          updateFocus

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          if BFm.allFieldsValid st._stPopChatEditForm
            then do
              let f = BFm.formState st._stPopChatEditForm
              case f._ceiSelectedModel of
                Just model ->
                  st._stPopChatEditOnOk f._ceiName model f._ceiParams
                Nothing -> do
                  store <- use C.stStore
                  liftIO . store.swLog.lgError $ "No model selected"
              clear
            else do
              pass

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

        _ -> B.zoom C.stPopChatEditForm $ BFm.handleFormEvent ev

      pass
    _ -> pass

  where
    clear = do
      C.stPopup .= Nothing
      C.stPopChatEditTitle .= Nothing
      C.stPopChatEditFocus %= BF.focusSetCurrent C.NPopChatEditFormName

    updateFocus :: B.EventM C.Name C.UiState ()
    updateFocus = do
      st <- B.get
      let focused = BF.focusGetCurrent st._stPopChatEditFocus

      case focused of
        Nothing -> pass
        Just C.NDialogOk -> pass
        Just C.NDialogCancel -> pass
        Just n -> C.stPopChatEditForm %= BFm.setFormFocus n
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Prompt Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupPrompt :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupPrompt _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopPromptFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        (_, Vty.KChar '\t', []) -> do
          C.stPopPromptFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopPromptFocus %= BF.focusPrev

        (Just C.NPopPromptEdit, Vty.KChar 'k', [Vty.MCtrl]) -> do
          C.stPopPromptEdit . BE.editContentsL %= TxtZ.clearZipper

        (Just C.NPopPromptEdit, Vty.KEnter, []) -> do
          accept st

        (Just C.NPopPromptEdit, _, _) -> do
          B.zoom C.stPopPromptEdit $ BE.handleEditorEvent ev

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          accept st

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        _ -> pass

    _ -> pass

  where
    accept st = do
      txt <- use (C.stPopPromptEdit . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)
      C.stPopup .= Nothing
      C.stPopPromptTitle .= Nothing
      C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit
      st._stPopPromptOnOk txt
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Confirm Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupConfirm :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupConfirm _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopConfirmFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          clear

        (_, Vty.KEsc, []) -> do
          clear

        (_, Vty.KChar '\t', []) -> do
          C.stPopConfirmFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopConfirmFocus %= BF.focusPrev

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          _ <- st._stPopConfirmOnOk
          clear

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass
  where
    clear = do
      C.stPopup .= Nothing
      C.stPopConfirmTitle .= Nothing
      C.stPopConfirmDetail .= Nothing
      C.stPopConfirmFocus %= BF.focusSetCurrent C.NDialogCancel
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Help Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupHelp :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupHelp _commandChan _ev ve = do
  case ve of
    Vty.EvKey k ms -> do
      case (k, ms) of
        (Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (Vty.KDown, []) -> do
          B.vScrollBy (B.viewportScroll C.NHelpScroll) 1

        (Vty.KUp, []) -> do
          B.vScrollBy (B.viewportScroll C.NHelpScroll) (-1)

        (Vty.KPageUp, []) -> do
          B.vScrollPage (B.viewportScroll C.NHelpScroll) B.Up

        (Vty.KPageUp, [Vty.MCtrl]) -> do
          B.vScrollToBeginning (B.viewportScroll C.NHelpScroll)

        (Vty.KPageDown, []) -> do
          B.vScrollPage (B.viewportScroll C.NHelpScroll) B.Down

        (Vty.KPageDown, [Vty.MCtrl]) -> do
          B.vScrollToEnd (B.viewportScroll C.NHelpScroll)

        _ -> pass

    _ -> pass
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Context Menu Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupContext :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupContext _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopContextFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopContextFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopContextFocus %= BF.focusPrev

        (Just C.NPopContextList, Vty.KEnter, []) -> do
          ok st

        (Just C.NPopContextList, _, _) -> do
          B.zoom C.stPopContextList $ BL.handleListEventVi BL.handleListEvent ve

        (Just C.NDialogOk, Vty.KEnter, []) ->
          ok st

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass

  where
    ok st = do
      case BL.listSelectedElement st._stPopContextList of
        Nothing -> pass
        Just (_, (n, _)) -> do
          clear
          st._stPopContextOnOk n

    clear = do
      C.stPopup .= Nothing
      C.stPopContextTitle .= Nothing
      C.stPopContextList .= BL.list C.NPopContextList (V.fromList []) 1
      C.stPopContextFocus %= BF.focusSetCurrent C.NPopContextList
      C.stPopContextOnOk .= const pass
 ----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Export Popup
----------------------------------------------------------------------------------------------------------------------
handleEventPopupExport :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupExport _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopExportFocus

  C.stDebug .= ""
  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopExportFocus %= BF.focusNext
          updateBrowserFromInputs

        (_, Vty.KBackTab, []) -> do
          C.stPopExportFocus %= BF.focusPrev
          updateBrowserFromInputs

        -- Suppress the space key / select file
        (Just C.NPopExportBrowser, Vty.KChar ' ', _) -> do
          pass
        --
        -- Suppress the search key for now
        (Just C.NPopExportBrowser, Vty.KChar '/', _) -> do
          pass

        -- Enter = go to dir
        (Just C.NPopExportBrowser, Vty.KEnter, _) -> do
          goToDir

        (Just C.NPopExportFormatJson, Vty.KEnter, []) -> do
          C.stPopExportFormat .= C.ExportJson

        (Just C.NPopExportFormatJson, Vty.KChar ' ', []) -> do
          C.stPopExportFormat .= C.ExportJson

        (Just C.NPopExportFormatText, Vty.KEnter, []) -> do
          C.stPopExportFormat .= C.ExportText

        (Just C.NPopExportFormatText, Vty.KChar ' ', []) -> do
          C.stPopExportFormat .= C.ExportText

        (Just C.NPopExportBrowser, _, _) -> do
          C.stPopExportError .= Nothing
          B.zoom C.stPopExportBrowser $ BFi.handleFileBrowserEvent ve
          updateFromBrowser

        (Just C.NPopExportFileName, _, _) -> do
          B.zoom C.stPopExportFName $ BE.handleEditorEvent ev

        (Just C.NPopExportDir, _, _) -> do
          B.zoom C.stPopExportDir $ BE.handleEditorEvent ev

        (Just C.NDialogOk, Vty.KEnter, []) ->
          ok

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass

  where
    ok :: B.EventM C.Name C.UiState ()
    ok = do
      dir <- use (C.stPopExportDir . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)
      fname <- use (C.stPopExportFName . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

      case (Txt.null dir, Txt.null fname) of
        (False, False) -> do
          st <- B.get
          clear
          st._stPopExportOnOk st._stPopExportFormat $ Txt.unpack dir </> Txt.unpack fname

        _ -> do
          C.stPopExportError .= Just "Empty dir or file name"


    goToDir = do
      browser <- use C.stPopExportBrowser
      case BFi.fileBrowserCursor browser of
        Nothing -> pass
        Just h -> do
          liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
            False -> pass
            True -> do
              browser2 <- liftIO $ BFi.setWorkingDirectory h.fileInfoFilePath browser
              C.stPopExportBrowser .= browser2


    clear = do
      st <- B.get
      browser2 <- liftIO $ BFi.newFileBrowser (const True) C.NPopExportBrowser (st._stAppConfig.acDefaultExportDir)
      let dir = BFi.getWorkingDirectory browser2
      C.stPopup .= Nothing
      C.stPopExportFocus %= BF.focusSetCurrent C.NPopExportBrowser
      C.stPopExportOnOk .= (const . const $ pass)
      C.stPopExportBrowser .= browser2
      C.stPopExportError .= Nothing
      C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing
      C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack dir] Nothing


    updateFromBrowser = do
      browser <- use C.stPopExportBrowser
      case BFi.fileBrowserCursor browser of
        Nothing -> do
          C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [] Nothing
          C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing
        Just h -> do
          liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
            True -> do
              C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack h.fileInfoFilePath] Nothing
              C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing

            False -> do
              C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack . Fp.takeDirectory $ h.fileInfoFilePath] Nothing
              C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [Txt.pack $ h.fileInfoSanitizedFilename] Nothing


    updateBrowserFromInputs = do
      st <- B.get
      case BF.focusGetCurrent st._stPopExportFocus of
        Just C.NPopExportBrowser -> do
          browser1 <- use C.stPopExportBrowser

          prevDir <-
            case BFi.fileBrowserCursor browser1 of
              Nothing -> pure Nothing
              Just h -> do
                liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
                  True -> pure . Just . Txt.pack $ h.fileInfoFilePath
                  False -> pure . Just . Txt.pack $ Fp.takeDirectory h.fileInfoFilePath

          dir <- use (C.stPopExportDir . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

          when (not (Txt.null dir) || Just dir /= prevDir) $ do
            browser2 <- liftIO $ BFi.setWorkingDirectory (Txt.unpack dir) browser1
            C.stPopExportBrowser .= browser2

        _ -> do
          pass

 ----------------------------------------------------------------------------------------------------------------------
