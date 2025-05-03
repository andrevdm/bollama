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

import Brick ((<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Debounce as Deb
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Control.Lens ((^.), (^?), (%=), (.=), (<>=), use, to, at, non)
import Data.List (findIndex)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Core qualified as C
import Config qualified as Cfg
import Utils qualified as U


---------------------------------------------------------------------------------------------------
-- Main Event Handler
---------------------------------------------------------------------------------------------------
handleEvent :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> B.EventM C.Name C.UiState ()
handleEvent commandChan ev = do
  case ev of
    B.AppEvent ae -> handleAppEvent commandChan ae
    B.VtyEvent ve -> do
      use C.stPopup >>= \case
        Nothing -> handleEventNoPopup commandChan ev ve
        Just C.PopupChatEdit -> handleEventPopupChatEdit commandChan ev ve

    _ -> pass




handleEventNoPopup :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventNoPopup commandChan ev ve = do
  case ve of
    Vty.EvKey k ms -> do
      st <- B.get
      let
        footerWidgetName = fst <$> st._stFooterWidget
        focused =
          case st._stTab of
            C.TabModels -> BF.focusGetCurrent st._stFocusModels
            C.TabPs -> BF.focusGetCurrent st._stFocusPs
            C.TabChat -> BF.focusGetCurrent st._stFocusChat
            C.TabColours -> Nothing


      case (st._stTab, footerWidgetName, focused, k, ms) of
        ---------------------------------------------------------------------------------------------------
        -- Global
        ---------------------------------------------------------------------------------------------------
        (_, _, _, Vty.KChar 'q', [Vty.MCtrl]) -> B.halt

        (_, _, _, Vty.KChar 'u', [Vty.MCtrl]) -> do
          (_es, m) <- liftIO $ U.attrMapFromFile "defaultAttrs.csv"
          C.stAttrMap .= m
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- Footer widget active
        ---------------------------------------------------------------------------------------------------
        (C.TabModels, Just C.NModelEditSearch, _, _, _) -> handleFooterWidgetModelSearch commandChan ev k ms
        (C.TabModels, Just C.NModelEditTag, _, _, _) -> handleFooterWidgetModelTag commandChan ev k ms
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- Function keys
        ---------------------------------------------------------------------------------------------------
        (_, _, _, Vty.KFun 2, []) -> do
          unless (st._stTab == C.TabModels) $ do
            C.stLoadingPs .= True
            C.stTab .= C.TabModels

        (_, _, _, Vty.KFun 3, []) -> do
          unless (st._stTab == C.TabPs) $ do
            liftIO . BCh.writeBChan commandChan $ C.CmdRefreshPs
            C.stLoadingPs .= True
            C.stTab .= C.TabPs

        (_, _, _, Vty.KFun 4, []) -> do
          unless (st._stTab == C.TabChat) $ do
            C.stLoadingPs .= True
            C.stTab .= C.TabChat

        (_, _, _, Vty.KFun 11, []) -> do
            C.stTab .= C.TabColours
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- Tabs
        ---------------------------------------------------------------------------------------------------
        (C.TabModels, _, _, _, _) -> handleTabModels commandChan ev ve focused k ms
        (C.TabPs, _, _, _, _) -> handleTabPs commandChan ev ve focused k ms
        (C.TabChat, _, _, _, _) -> handleTabChat commandChan st._stStore ev ve focused k ms
        (C.TabColours, _, _, _, _) -> handleTabColours commandChan ev ve focused k ms
        ---------------------------------------------------------------------------------------------------

        _ -> pass
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
  C.stPopChatEditModels %= BL.listMoveTo 0 . BL.listReplace (V.fromList ms) Nothing
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
  C.stPopChatEditModels %= BL.listMoveTo 0 . BL.listReplace (V.fromList vs2) Nothing


handleAppEventGotTime :: BCh.BChan C.Command -> DT.UTCTime -> B.EventM C.Name C.UiState ()
handleAppEventGotTime _commandChan t = do
  C.stTime .= t


handleChatUpdated :: C.ChatId -> B.EventM C.Name C.UiState ()
handleChatUpdated chatId  = do
  use C.stChatCurrent >>= \case
    Just (currentChatId, streamingStateOld) | currentChatId == chatId -> do
      store <- use C.stStore
      (ms, streamingState) <- liftIO store.swGetCurrent >>= \case
        Nothing -> pure ([], streamingStateOld)
        Just (_, _, ss, ms') -> pure (ms', ss)

      C.stChatCurrent .= Just (currentChatId, streamingState)
      C.stChatMsgList %= BL.listMoveToEnd . BL.listReplace (V.fromList . reverse $ ms) Nothing

    _ -> do
      pass


handleChatStreamResponseDone :: BCh.BChan C.Command -> C.ChatId -> B.EventM C.Name C.UiState ()
handleChatStreamResponseDone commandChan chatId = do
  -- Save to store
  store <- use C.stStore
  liftIO $ store.swStreamDone chatId

  -- Update with the final response
  --  Some updates may have been suppressed by the debounce
  --  This also updates the stCurrent's streaming status
  handleChatUpdated chatId
---------------------------------------------------------------------------------------------------





---------------------------------------------------------------------------------------------------
-- Footer Events
---------------------------------------------------------------------------------------------------
handleFooterWidgetModelSearch
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleFooterWidgetModelSearch _commandChan ev k ms =
  case (k, ms) of
    (Vty.KEsc, []) -> do
      C.stFooterWidget .= Nothing

    (Vty.KEnter, []) -> do
      txt <- use (C.stModelFilterEditor . BE.editContentsL . to TxtZ.getText)
      C.stModelsFilter .= Txt.unlines txt
      C.stFooterWidget .= Nothing
      filteredModels <- filterModels
      wasSelected <- B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName)
      let ix = findIndex (\x -> Just x.miName == wasSelected) filteredModels
      C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix

    (_, _) -> do
      B.zoom C.stModelFilterEditor $ BE.handleEditorEvent ev



handleFooterWidgetModelTag
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleFooterWidgetModelTag _commandChan ev k ms =
  case (k, ms) of
    (Vty.KEsc, []) -> do
      C.stFooterWidget .= Nothing

    (Vty.KEnter, []) -> do
      B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
        Nothing -> pass
        Just selected -> do
          txt <- use (C.stModelTagEditor . BE.editContentsL . to TxtZ.getText)
          C.stAppConfig %= \cfg ->
            cfg { C.acModelTag = Map.insert selected (Txt.unlines txt) cfg.acModelTag }
          liftIO . Cfg.writeAppConfig =<< use C.stAppConfig

      C.stModelTagEditor . BE.editContentsL %= TxtZ.clearZipper
      C.stFooterWidget .= Nothing

    (_, _) -> do
      B.zoom C.stModelTagEditor $ BE.handleEditorEvent ev
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
handleTabModels _commandChan _ev ve focused k ms =
  case (focused, k, ms) of
    (Just C.NModelsList, Vty.KChar '/', []) -> do
      C.stFooterWidget .= Just (C.NModelEditSearch, \st2 -> (B.withAttr (B.attrName "footerTitle") $ B.txt "filter: ") <+> BE.renderEditor (B.txt . Txt.unlines) True st2._stModelFilterEditor)

    (Just C.NModelsList, Vty.KChar 't', []) -> do
      cfg <- use C.stAppConfig
      tags <-
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to C.miName) >>= \case
          Nothing -> pure ""
          Just selected -> do
            pure . fromMaybe "" $ Map.lookup selected cfg.acModelTag

      C.stModelTagEditor .= BE.editorText C.NModelEditTag (Just 1) tags
      C.stFooterWidget .= Just (C.NModelEditTag, \st2 -> (B.withAttr (B.attrName "footerTitle") $ B.txt "tag: ") <+> BE.renderEditor (B.txt . Txt.unlines) True st2._stModelTagEditor)

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
      C.stPopChatEditOnOk .= \name model -> do
        chat <- store.swNewChat name model C.SsNotStreaming
        BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just chat.chatId)


    (Just C.NChatInputEdit, Vty.KChar 'r', [Vty.MCtrl]) -> do
      runInput

    (Just C.NChatInputEdit, Vty.KFun 5, []) -> do
      runInput

    (Just C.NChatInputEdit, _, _) -> do
      --C.stDebug .= show (k, ms)
      B.zoom C.stChatInput $ BE.handleEditorEvent ev

    (Just C.NChatMsgList, _, _) -> do
      --C.stDebug .= show (k, ms)
      B.zoom C.stChatMsgList $ BL.handleListEventVi BL.handleListEvent ve

    (Just C.NChatsList, _, _) -> do
      B.zoom C.stChatsList $ BL.handleListEventVi BL.handleListEvent ve
      handleChatSelectionUpdate

    _ -> pass

  where
    runInput = do
      let chatModel = "qwen3:0.6b" --TODO
      let chatName = "chatX" --TODO

      cid <-
        liftIO store.swGetCurrent >>= \case
          Just (cid', _, _strmState, _) -> pure cid'
          Nothing -> do
            chat <- liftIO $ store.swNewChat chatName chatModel C.SsNotStreaming --TODO why
            _ <- liftIO $ store.swSetCurrent (Just chat.chatId)
            pure chat.chatId

      txt <- use (C.stChatInput . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

      unless (Txt.null txt) $ do
        liftIO (store.swAddMessage cid O.User C.SsStreaming chatModel txt) >>= \case
          Right newMsg -> do
            C.stChatCurrent .= Just (cid, C.SsStreaming)
            C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
            handleChatUpdated cid

            liftIO . BCh.writeBChan commandChan $ C.CmdChatSend cid newMsg


          Left err -> do
            C.stDebug .= "error: " <> err
            --TODO


handleChatSelectionUpdate :: B.EventM C.Name C.UiState ()
handleChatSelectionUpdate = do
  selectedChat' <- use (C.stChatsList . to BL.listSelectedElement)
  prevSelected' <- use C.stChatCurrent
  store <- use C.stStore

  case (selectedChat', prevSelected') of
    -- No change, nothing to do
    (Just (_, selectedChat), Just (prevChatId, _)) | selectedChat.chatId == prevChatId -> do
      pass

    -- Nothing selected, clear current
    (Nothing, _) -> do
      C.stChatCurrent .= Nothing
      C.stChatMsgList %= BL.listClear
      _ <- liftIO $ store.swSetCurrent Nothing
      pass

    (Just (_, selectedChat), _)-> do
      liftIO (store.swSetCurrent (Just selectedChat.chatId)) >>= \case
        Just (_, _, streamingState) -> do
          C.stChatCurrent .= Just (selectedChat.chatId, streamingState)
          handleChatUpdated selectedChat.chatId

        Nothing -> do
          C.stChatCurrent .= Nothing
          C.stChatMsgList %= BL.listClear


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


    C.CmdChatSend chatId msg -> do
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
              , options = Nothing
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
            r <- O.chat ops
            --TODO check response
            pass

        Nothing -> do
          --TODO send message for error
          pass

    C.CmdRefreshChatsList overrideSelect -> do
      chats <- store.swListChats
      BCh.writeBChan eventChan $ C.UeGotChatsList chats overrideSelect


      pass


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

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopChatEditFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopChatEditFocus %= BF.focusPrev

        (Just C.NPopChatEditName, _, _) -> do
          B.zoom C.stPopChatEditName $ BE.handleEditorEvent ev

        (Just C.NPopChatEditModels, _, _) -> do
          B.zoom C.stPopChatEditModels $ BL.handleListEventVi BL.handleListEvent ve

        (Just C.NPopChatEditOk, Vty.KEnter, []) -> do
          chatName <- use (C.stPopChatEditName . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)
          chatModel <- use (C.stPopChatEditModels . BL.listSelectedElementL . to (.miName))
          C.stPopup .= Nothing
          C.stPopChatEditName . BE.editContentsL %= TxtZ.clearZipper
          C.stPopChatEditFocus %= BF.focusSetCurrent C.NPopChatEditName
          liftIO $ st._stPopChatEditOnOk chatName chatModel

        (Just C.NPopChatEditCancel, Vty.KEnter, []) -> do
          C.stPopChatEditName . BE.editContentsL %= TxtZ.clearZipper
          C.stPopChatEditFocus %= BF.focusSetCurrent C.NPopChatEditName
          C.stPopup .= Nothing

        _ -> pass

      pass
    _ -> pass
