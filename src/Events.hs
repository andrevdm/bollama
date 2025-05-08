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

import Brick ((<=>), (<+>))
import Brick qualified as B
import Brick.BChan qualified as BCh
import Brick.Forms qualified as BFm
import Brick.Focus qualified as BF
import Brick.Widgets.Edit qualified as BE
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
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Config qualified as Cfg
import Core qualified as C
import Draw qualified as D
import Utils qualified as U


---------------------------------------------------------------------------------------------------
-- Main Event Handler
---------------------------------------------------------------------------------------------------
handleEvent :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> B.EventM C.Name C.UiState ()
handleEvent commandChan ev = do
  catch
    (do
      case ev of
        -- App events are global and must always be handled
        B.AppEvent ae -> handleAppEvent commandChan ae

        -- Handle mouse events for the scrollbar
        B.MouseDown (C.VScrollClick se C.NChatScroll) _ _ _ -> do
          case  se of
            B.SBHandleBefore -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up
            B.SBHandleAfter -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down
            B.SBTroughBefore -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up
            B.SBTroughAfter -> B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down
            _ -> pass


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
                -- Otherwise the main UI gets the event
                Nothing -> handleEventNoPopup commandChan ev ve

        _ -> pass
    )
    (\(_ :: SomeException) -> do
      st <- B.get
      liftIO $ st._stLog.lgCritical $ "Exception in event handler: " <> show ev
    )




handleEventNoPopup :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventNoPopup commandChan ev ve = do
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
        (C.TabChat, _, _, _) -> handleTabChat commandChan st._stStore ev ve focused k ms
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
    C.UeTick t -> handleAppEventTick commandChan t
    C.UeGotModelList ms1 -> handleAppEventGotModelList commandChan ms1
    C.UePsList ps' -> handleAppEventPsList commandChan ps'
    C.UeGotModelShow x -> handleAppEventGotModelShow commandChan x
    C.UeModelShowDone -> handleAppEventModelShowDone commandChan
    C.UeGotTime t -> handleAppEventGotTime commandChan t
    C.UeChatUpdated chatId -> handleChatUpdated chatId
    C.UeChatStreamResponseDone chatId -> handleChatStreamResponseDone commandChan chatId

    C.UeGotChatsList chats overrideSelect' -> do
      C.stChatsList %= BL.listReplace (V.fromList chats) Nothing

      case overrideSelect' of
        Just chatId -> C.stChatsList %= BL.listFindBy (\i -> i.chatId == chatId)
        Nothing -> C.stChatsList %= BL.listMoveTo 0

      handleChatSelectionUpdate

    C.UeLogUpdated ls (lvl, msg) -> do
      C.stLogList %= BL.listReplace (V.fromList ls) Nothing
      C.stLogList %= BL.listMoveToEnd

      C.stDebug .= (U.logLevelName lvl) <> ":" <> msg

      when (lvl `elem` [C.LlCritical, C.LlError]) $ do
        C.stErrorMessage .= Just msg


handleAppEventTick :: BCh.BChan C.Command -> Int -> B.EventM C.Name C.UiState ()
handleAppEventTick commandChan t = do
  C.stTick .= t
  currentTab <- use C.stTab

  when (t `mod` 50 == 0 && currentTab == C.TabPs) $ do
    liftIO (BCh.writeBChan commandChan C.CmdRefreshPs)


handleAppEventGotModelList :: BCh.BChan C.Command -> [O.ModelInfo] -> B.EventM C.Name C.UiState ()
handleAppEventGotModelList commandChan ms1 = do
  cfg <- use C.stAppConfig
  let ms2 = ms1
  let ms = ms2 <&> \m -> C.ModelItem
        { miName = m.name
        , miInfo = m
        , miShow = Nothing
        , miTag = fromMaybe "" $ cfg.acModelTag ^. at m.name
        }

  C.stModels .= ms
  filteredModels <- filterModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) Nothing
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
  vs1 <- use C.stModels
  let
    ix = findIndex (\x -> x.miName == m) vs1
    vs2 = vs1 <&> \old ->
      if old.miName == m
        then old { C.miShow = Just s }
        else old

  C.stModels .= vs2
  filteredModels <- filterModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix


handleAppEventModelShowDone :: BCh.BChan C.Command -> B.EventM C.Name C.UiState ()
handleAppEventModelShowDone _commandChan = do
  l1 <- use C.stModelsList
  let
    selected = snd <$> BL.listSelectedElement l1
    selectedName = (.miName) <$> selected
    vs1 = V.toList $ BL.listElements l1
    vs2 = reverse $ sortOn U.parseParams vs1
    ix = findIndex (\x -> Just x.miName == selectedName) vs2

  C.stModels .= vs2
  filteredModels <- filterModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix
  C.stModelShowLoading .= False


handleAppEventGotTime :: BCh.BChan C.Command -> DT.UTCTime -> B.EventM C.Name C.UiState ()
handleAppEventGotTime _commandChan t = do
  C.stTime .= t


handleChatUpdated :: C.ChatId -> B.EventM C.Name C.UiState ()
handleChatUpdated chatId  = do
  use C.stChatCurrent >>= \case
    Just (currentChat, streamingStateOld) | currentChat.chatId == chatId -> do
      store <- use C.stStore
      (ms, streamingState) <- liftIO store.swGetCurrent >>= \case
        Nothing -> pure ([], streamingStateOld)
        Just (_, _, ss, ms') -> pure (ms', ss)

      C.stChatCurrent .= Just (currentChat, streamingState)
      C.stChatMsgs .= reverse ms
      B.vScrollToEnd (B.viewportScroll C.NChatScroll)

    _ -> do
      pass


handleChatStreamResponseDone :: BCh.BChan C.Command -> C.ChatId -> B.EventM C.Name C.UiState ()
handleChatStreamResponseDone _commandChan chatId = do
  -- Save to store
  store <- use C.stStore
  liftIO $ store.swStreamDone chatId

  -- Update with the final response
  --  Some updates may have been suppressed by the debounce
  --  This also updates the stCurrent's streaming status
  handleChatUpdated chatId
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

    (Just C.NModelsList, Vty.KChar 't', []) -> do
      tags <- do
        cfg <- use C.stAppConfig
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
          Nothing -> pure ""
          Just selected -> pure . maybe "" (Txt.strip . Txt.replace "\n" " " . Txt.replace "\r" " ") $ Map.lookup selected cfg.acModelTag

      C.stPopup .= Just C.PopupPrompt
      C.stPopPromptEdit . BE.editContentsL .= TxtZ.textZipper [tags] Nothing
      C.stPopPromptTitle .= Just "User tag edit"
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
                 liftIO $ O.deleteModel selected
                 liftIO $ BCh.writeBChan commandChan C.CmdRefreshModelList
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting model: " <> show e
              )

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
handleTabPs _commandChan _ev ve focused k ms =
  case (focused, k, ms) of
    (Just C.NListPs, Vty.KChar 's', []) -> do
      B.gets (^?  C.stPs . BL.listSelectedElementL . to (.modelName)) >>= \case
        Nothing -> pass
        Just name -> do
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
            , hostUrl = Just C.ollamaUrl
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
  -> C.StoreWrapper
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabChat commandChan store ev ve focused k ms =
  case (focused, k, ms) of
    (_, Vty.KChar '\t', []) -> do
      C.stFocusChat %= BF.focusNext

    (_, Vty.KBackTab, []) -> do
      C.stFocusChat %= BF.focusPrev

    (_, Vty.KChar 'n', [Vty.MCtrl]) -> do
      C.stPopup .= Just C.PopupChatEdit
      C.stPopChatEditTitle .= Just "New chat"
      C.stPopChatEditOnOk .= \name model -> do
        chat <- liftIO $ store.swNewChat name model C.SsNotStreaming
        liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)

    (_, Vty.KChar 'e', [Vty.MCtrl]) -> do
      use C.stChatCurrent >>= \case
        Nothing -> pass
        Just (chat, ss) -> do
          st <- B.get
          C.stPopChatEditForm .= BFm.setFormConcat (D.vBoxWithPadding 1) (D.mkPopChatEditForm C.ChatEditInfo
            { _ceiContext = 2048 --TODO
            , _ceiTemp = 0.8 --TODO
            , _ceiModels = st._stModels
            , _ceiSelectedModel = find (\i -> i.miName == chat.chatModel) st._stModels
            , _ceiName = chat.chatName
            })

          C.stPopup .= Just C.PopupChatEdit
          C.stPopChatEditTitle .= Just "Edit chat"
          C.stPopChatEditOnOk .= \name model -> do
            let chat2 = chat { C.chatName = name, C.chatModel = model }
            liftIO $ store.swSaveChat chat2
            C.stChatCurrent .= Just (chat2, ss)
            liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)

    (_, Vty.KPageUp, []) -> do
      C.stDebug .= "PageUp"
      B.vScrollPage (B.viewportScroll C.NChatScroll) B.Up

    (_, Vty.KPageUp, [Vty.MCtrl]) -> do
      C.stDebug .= "PageUp"
      B.vScrollToBeginning (B.viewportScroll C.NChatScroll)

    (_, Vty.KPageDown, []) -> do
      C.stDebug .= "PageDown"
      B.vScrollPage (B.viewportScroll C.NChatScroll) B.Down

    (_, Vty.KPageDown, [Vty.MCtrl]) -> do
      C.stDebug .= "PageDown"
      B.vScrollToEnd (B.viewportScroll C.NChatScroll)

    (Just C.NChatInputEdit, Vty.KChar 'r', [Vty.MCtrl]) -> do
      runInput

    (Just C.NChatInputEdit, Vty.KChar 's', [Vty.MCtrl]) -> do
      runInput

    (Just C.NChatInputEdit, Vty.KFun 5, []) -> do
      runInput

    (Just C.NChatInputEdit, _, _) -> do
      C.stDebug .= show (k, ms)
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
    runInput = do
      liftIO store.swGetCurrent >>= \case
        Just (cid, chat, _strmState, _) -> do
          txt <- use (C.stChatInput . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

          unless (Txt.null txt) $ do
            findModel chat.chatModel >>= \case
              Just model -> do
                liftIO (store.swAddMessage cid O.User C.SsStreaming chat.chatModel txt) >>= \case
                  Right newMsg -> do
                    C.stChatCurrent .= Just (chat, C.SsStreaming)
                    C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                    handleChatUpdated cid

                    let ctxLen =
                         case model.miShow of
                           Nothing -> Nothing
                           Just ms' -> ms'.modelInfo.llamaContextLength

                    liftIO . BCh.writeBChan commandChan $ C.CmdChatSend cid newMsg (fromMaybe 2048 ctxLen)

                  Left err -> do
                    liftIO . store.swLog.lgError $ "Error sending message: " <> err

              Nothing -> do
                C.stDebug .= "Invalid model name: " <> chat.chatModel

                st <- B.get
                C.stPopChatEditForm .= BFm.setFormConcat (D.vBoxWithPadding 1) (D.mkPopChatEditForm C.ChatEditInfo
                  { _ceiContext = 2048 --TODO
                  , _ceiTemp = 0.8 --TODO
                  , _ceiModels = st._stModels
                  , _ceiSelectedModel = find (\i -> i.miName == chat.chatModel) st._stModels
                  , _ceiName = chat.chatName
                  })


                C.stPopup .= Just C.PopupChatEdit
                C.stPopChatEditTitle .= Just "Select a model"
                C.stPopChatEditOnOk .= \name model -> do
                  let chat2 = chat { C.chatName = name, C.chatModel = model }
                  liftIO $ store.swSaveChat chat2
                  C.stChatCurrent %= \case
                    Just (_, ss) -> Just (chat2, ss)
                    Nothing -> Nothing


        Nothing -> do
          liftIO $ store.swLog.lgError "No current chat"


    findModel n = do
      models <- use C.stModels
      pure $ find (\m -> m.miName == n) models



handleChatSelectionUpdate :: B.EventM C.Name C.UiState ()
handleChatSelectionUpdate = do
  selectedChat' <- use (C.stChatsList . to BL.listSelectedElement)
  prevSelected' <- use C.stChatCurrent
  store <- use C.stStore

  case (selectedChat', prevSelected') of
    -- No change, nothing to do
    (Just (_, selectedChat), Just (prevChatId, _)) | selectedChat.chatId == prevChatId.chatId -> do
      pass

    -- Nothing selected, clear current
    (Nothing, _) -> do
      C.stChatCurrent .= Nothing
      C.stChatMsgs .= []
      _ <- liftIO $ store.swSetCurrent Nothing
      pass

    (Just (_, selectedChat), _)-> do
      liftIO (store.swSetCurrent (Just selectedChat.chatId)) >>= \case
        Just (_, _, streamingState) -> do
          C.stChatCurrent .= Just (selectedChat, streamingState)
          handleChatUpdated selectedChat.chatId

        Nothing -> do
          C.stChatCurrent .= Nothing
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
      C.stDebug .= show (k, ms)
      B.zoom C.stLogList $ BL.handleListEventVi BL.handleListEvent ve
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------
filterModels :: B.EventM C.Name C.UiState [C.ModelItem]
filterModels = do
  vs <- use C.stModels
  t <- Txt.strip . Txt.toLower <$> use C.stModelsFilter

  if Txt.null t
    then pure vs
    else
      pure $ filter (\mi ->
          let
            name = Txt.strip . Txt.toLower $ mi.miName
            capabilities = Txt.toLower . Txt.intercalate " " . fromMaybe [] . join $ mi.miShow <&> (.capabilities)
            tags = Txt.toLower mi.miTag
          in
          Txt.isInfixOf t (name <> " " <> capabilities <> " " <> tags)
        )
        vs
---------------------------------------------------------------------------------------------------




----------------------------------------------------------------------------------------------------------------------
-- background thread
----------------------------------------------------------------------------------------------------------------------
runCommands :: BCh.BChan C.Command -> BCh.BChan C.UiEvent -> C.StoreWrapper -> IO ()
runCommands commandChan eventChan store = forever $ do
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


    C.CmdChatSend chatId msg ctxLen -> do
      store.swGetChat chatId >>= \case
        Just (chat, hist1, _streamState) -> do
          debouncedUpdateUi <- Deb.mkDebounce Deb.defaultDebounceSettings
            { Deb.debounceFreq = 500_000  -- 500ms
            , Deb.debounceEdge = Deb.leadingEdge
            , Deb.debounceAction = BCh.writeBChan eventChan $ C.UeChatUpdated chatId
            }

          streamingMessageId <- liftIO $ C.MessageId <$> U.newUuidText

          let
            hist2 = hist1 <&> \m -> O.Message m.msgRole m.msgText Nothing Nothing
            oMsg = O.Message msg.msgRole msg.msgText Nothing Nothing
            msgAndCtx = NE.fromList $ (reverse hist2) <> [oMsg]

            ops = O.ChatOps
              { chatModelName = chat.chatModel
              , messages = msgAndCtx
              , tools = Nothing
              , format = Nothing
              , keepAlive = Just "5m"
              , hostUrl = Just C.ollamaUrl
              , responseTimeOut = Nothing
              , options = Just . Ae.object $
                [ ("num_ctx", Ae.Number . fromIntegral $ ctxLen)
                ]
              , stream = Just (
                \cr -> do
                  case cr.message of
                    Nothing -> pass
                    Just m -> do
                      _ <- liftIO $ store.swAddStreamedChatContent chatId streamingMessageId m.role m.content
                      debouncedUpdateUi

                  when (cr.done) $ do
                    BCh.writeBChan eventChan $ C.UeChatStreamResponseDone chatId

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
              (\(_ :: SomeException) -> do
                store.swLog.lgError $ "Exception in chat stream: \n" <> show chatId
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
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

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
              C.stPopup .= Nothing
              C.stPopChatEditTitle .= Nothing
              let f = BFm.formState st._stPopChatEditForm
              st._stPopChatEditOnOk f._ceiName (maybe "" C.miName f._ceiSelectedModel)
            else do
              pass

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

        _ -> B.zoom C.stPopChatEditForm $ BFm.handleFormEvent ev

      pass
    _ -> pass

  where
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
