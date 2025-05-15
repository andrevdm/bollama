{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.TabChat
  ( drawTabChat
  , handleTabChat
  , handleButtonDown
  , startNewChat
  , handleChatSelectionUpdate
  , handleChatStreamResponseDone
  , saveStoreCurrentChat
  , changeChatToStoreCurrent
  , handleChatUpdated
  , gotChatsList
  , chatSend
  , refreshChatsList
  ) where

import Verset
import Brick ((<=>), (<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick.Forms qualified as BFm
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Concurrent.STM.TVar qualified as TV
import Control.Debounce as Deb
import Control.Exception.Safe (catch)
import Control.Lens ((^.), (%=), (.=), use, to)
import Data.Aeson qualified as Ae
import Data.Char qualified as Chr
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text.IO qualified as Txt
import Data.Text.Lazy qualified as TxtL
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O
import System.Hclip qualified as Clip
import Text.Emoji qualified as Emj
import Text.Printf (printf)
import Text.Unidecode qualified as Uni

import Core qualified as C
import Config qualified as Cfg
import Logging qualified as L
import Messages qualified as M
import Storage.Store qualified as Sr
import Utils qualified as U
import Widgets.Common qualified as Wc
import Widgets.PopupContextMenu qualified as WPctx
import Widgets.PopupEditChat qualified as WPce


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawTabChat :: C.UiState -> B.Widget C.Name
drawTabChat st =
  drawChatTop
  <=>
  drawChatMain
  <=>
  drawChatBottom

  where
    drawChatMain =
      drawChatMainLeft <+> (B.padLeft (B.Pad 4) $ drawChatMainRight)

    drawChatTop =
      B.emptyWidget

    drawChatBottom =
      B.emptyWidget

    drawChatMainLeft =
      let selected = BF.focusGetCurrent st._stFocusChat == Just C.NChatsList
      in
      B.hLimit 30 $
      Wc.borderWithLabel' selected "Chats" $
      BL.renderList renderChatItem selected (st._stChatsList)

    drawChatMainRight =
      let
        inputEditSelected = BF.focusGetCurrent st._stFocusChat == Just C.NChatInputEdit
        modelName = fromMaybe "" $ st._stChatCurrent <&> (M.chatModel)
      in
      Wc.borderWithLabel' False "Conversation"
      ( (B.withAttr (B.attrName "colHeader") $ B.txt "Model: ") <+> (B.txt modelName)
        <=>
        B.txt " "
        <=>
        ( let ws = zip [0..] st._stChatMsgs <&> \(ix, msg) -> renderChatMsgItem False ix False msg
          in
          B.withClickableVScrollBars C.VScrollClick . B.withVScrollBarHandles . B.withVScrollBars B.OnRight $
          B.viewport C.NChatScroll B.Vertical . B.vBox $ ws
        )
      )
      <=>
      B.vLimit 8
      (
        Wc.borderWithLabel' inputEditSelected "Input (ctrl-s to send)"
        ( case M.chatStreaming <$> st._stChatCurrent of
            Nothing -> B.fill ' '
            Just M.SsNotStreaming -> (BE.renderEditor (B.txt . Txt.unlines) inputEditSelected st._stChatInput)
            Just M.SsStreaming -> Wc.spinner2 st <+> B.fill ' '
        )
      )


    renderChatMsgItem :: Bool -> Int -> Bool -> M.ChatMessage -> B.Widget C.Name
    renderChatMsgItem listSelected ix itemSelected msg =
      let attrName =
            if listSelected && itemSelected
            then "chatMsgSelected"
            else if ix `mod` 2 == 0 then "chatMsgA" else "chatMsgB"

          msgTxt =
            if st._stShowThinking
            then cleanText $ msg.msgText
            else cleanText $ U.removeThink msg.msgText
      in
      B.padBottom (B.Pad 1) $
      B.withAttr (B.attrName attrName) $
      B.hBox
        [ B.hBox
            [ B.vBox
                [ Wc.col 15 (show msg.msgRole) attrName
                , if st._stAppConfig.acAllowMouse
                  then B.txt " " <+> B.withAttr (B.attrName "copyText") (B.clickable (C.NChatMsgCopy msg.msgId) $ B.txt "[copy]")
                  else B.emptyWidget
                ]
            , B.txtWrap msgTxt
            ]
        , renderChatMsgDetail itemSelected msg
        ]

    cleanText :: Text -> Text
    cleanText t = transliterate . replaceWithColons $ t

    replaceWithColons :: Text -> Text
    replaceWithColons =
      Emj.replaceEmojis $ \_emoji aliases ->
        case aliases of
          (a:_) -> ":" <> a <> ":"
          []    -> "_"  -- unknown emoji

    transliterate :: Text -> Text
    transliterate = TxtL.toStrict . TxtL.pack . concatMap go . Txt.unpack
      where
        go = \case
          '\t' -> "   "
          c | Chr.ord c < 256 -> [c]
          c -> Uni.unidecode c


    renderChatMsgDetail :: Bool -> M.ChatMessage -> B.Widget C.Name
    renderChatMsgDetail _selected msg =
      if st._stShowMessageDetail
      then
        B.padAll 1 . BB.border $
        B.withAttr (B.attrName "msgDetailBlock") $
        B.vBox
          [ B.withAttr (B.attrName "msgDetailTitle") (B.txt "Model: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt msg.msgModel))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Created At: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (Txt.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M:%S" msg.msgCreatedAt)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Total Duration: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (formatDuration . join $ M.cdTotalDuration <$> msg.msgDetail)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Load Duration: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (formatDuration . join $ M.cdLoadDuration <$> msg.msgDetail)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Prompt Eval Count: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (showMaybe . join $ M.cdPromptEvalCount <$> msg.msgDetail)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Prompt Eval Duration: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (formatDuration . join $ M.cdLoadDuration <$> msg.msgDetail)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Eval Count: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (showMaybe . join $ M.cdEvalCount <$> msg.msgDetail)))
          , B.withAttr (B.attrName "msgDetailTitle") (B.txt "Eval Duration: ") <+> (B.withAttr (B.attrName "msgDetailText") (B.txt (formatDuration . join $ M.cdEvalDuration <$> msg.msgDetail)))
          ]
      else
        B.emptyWidget

    showMaybe :: Show a => Maybe a -> Text
    showMaybe Nothing = ""
    showMaybe (Just a) = show a

    formatDuration :: Maybe Int -> Text
    formatDuration Nothing = ""
    formatDuration (Just ns) =
      let
        totalMs = ns `div` 1_000_000
        (mins, remMs1) = totalMs `divMod` 60_000
        (secs, millis) = remMs1   `divMod` 1_000
      in
      Txt.pack $ printf "%02d:%02d.%03d" mins secs millis


    renderChatItem :: Bool -> M.Chat -> B.Widget C.Name
    renderChatItem _selected chat =
      let defaultMarker =
            if Just chat.chatName == st._stAppConfig.acDefaultChat || Just (M.unChatId chat.chatId) == st._stAppConfig.acDefaultChat
            then B.withAttr (B.attrName "chatDefaultMarker") $ B.txt "*"
            else B.txt " "
      in
      B.vLimit 1 $
      defaultMarker <+> B.txt chat.chatName
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleTabChat
  :: BCh.BChan C.Command
  -> BCh.BChan C.UiEvent
  -> Sr.StoreWrapper
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
          U.setFooterMessage 10 $ "Setting default chat: " <> chat.chatName
          C.stAppConfig %= \cfg2 -> cfg2 { C.acDefaultChat = Just (M.unChatId chat.chatId) }
          liftIO . Cfg.writeAppConfig =<< use C.stAppConfig


    (Just C.NChatsList, _, _) -> do
      B.zoom C.stChatsList $ BL.handleListEventVi BL.handleListEvent ve
      handleChatSelectionUpdate

    _ -> pass

  where
    contextMenu = do
      let menuItems =
           [ ("export", "e^xport chat")
           , ("edit", "^edit chat")
           , ("new", "^new chat")
           , ("think", "^toggle thinking")
           , ("detail", "toggle message detai^l")
           , ("stop", "^stop chat")
           , ("clear", "^remove all messages")
           , ("delete", "^delete chat")
           ]
      C.stPopup .= Just C.PopupContext
      C.stPopContextTitle .= Just "Chat"
      C.stPopContextList .= BL.list C.NPopContextList (V.fromList . WPctx.buildMenuItems $ menuItems) 1
      C.stPopContextOnOk .= \case
        "export" -> exportChat
        "edit" -> editModel "Edit chat"
        "new" -> startNewChat commandChan Nothing
        "think" -> C.stShowThinking %= not
        "detail" -> C.stShowMessageDetail %= not
        "stop" -> stopChat
        "clear" -> clearChatMessages
        "delete" -> deleteChat
        _ -> pass


    exportChat = do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
       Nothing -> U.setFooterMessage 10 "No chat selected"
       Just (_, chat1) -> do
          liftIO (store.swGetChat chat1.chatId) >>= \case
            Nothing -> U.setFooterMessage 10 "No chat found"
            Just (chat, ms') -> do
              C.stPopup .= Just C.PopupExport
              C.stPopExportOnOk .= \exportFormat path -> do
                liftIO $ M.exportChatToFile chat ms' exportFormat path
                U.setFooterMessage 10 $ "Exported chat to: " <> Txt.pack path


    deleteChat = do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
        Nothing -> pass
        Just (_, chat) -> do
          C.stPopup .= Just C.PopupConfirm
          C.stPopConfirmTitle .= Just "Are you sure you want to remove all messages for this chat?"
          C.stPopConfirmDetail .= Just chat.chatName
          C.stPopConfirmOnOk .= do
            catch
              (do
                 st <- B.get
                 C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                 liftIO $ st._stStore.swDeleteChat chat.chatId
                 liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)
                 U.setFooterMessage 10 $ "Deleted chat: " <> chat.chatName
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting chat messages: " <> show e
              )


    clearChatMessages = do
      use (C.stChatsList . to BL.listSelectedElement) >>= \case
        Nothing -> pass
        Just (_, chat) -> do
          C.stPopup .= Just C.PopupConfirm
          C.stPopConfirmTitle .= Just "Are you sure you want to delete this chat?"
          C.stPopConfirmDetail .= Just chat.chatName
          C.stPopConfirmOnOk .= do
            catch
              (do
                 st <- B.get
                 C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                 liftIO $ st._stStore.swDeleteAllChatMessages chat.chatId
                 liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)
                 U.setFooterMessage 10 $ "Deleted all chat messages: " <> chat.chatName
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting chat: " <> show e
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

         C.stPopChatEditForm .= BFm.setFormConcat (Wc.vBoxWithPadding 1) (WPce.mkPopChatEditForm M.ChatEditInfo
           { M._ceiModels = st._stModels
           , M._ceiSelectedModel = find (\i -> i.miName == chat.chatModel) st._stModels
           , M._ceiName = chat.chatName
           , M._ceiParams = chat.chatParams
           })

         C.stPopup .= Just C.PopupChatEdit
         C.stPopChatEditTitle .= Just title
         C.stPopChatEditOnOk .= \name model prms -> do
           let chat2 = chat
                { M.chatName = name
                , M.chatModel = model.miName
                , M.chatParams = prms
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
                liftIO (store.swAddMessage cid O.User M.SsStreaming chat.chatModel txt) >>= \case
                  Right newMsg -> do
                    C.stChatCurrent .= Just chat {M.chatStreaming = M.SsStreaming}
                    C.stChatInput . BE.editContentsL %= TxtZ.clearZipper
                    _ <- liftIO $ store.swClearChatInput cid
                    handleChatUpdated cid
                    liftIO . BCh.writeBChan commandChan $ C.CmdChatSend cid newMsg

                  Left err -> do
                    liftIO . store.swLog.lgError $ "Error sending message: " <> err

              Nothing -> do
                U.setFooterMessage 10 $ "Invalid model name: " <> chat.chatModel
                editModel "Select a model"


        Nothing -> do
          liftIO $ store.swLog.lgError "No current chat"


    findModel n = do
      models <- use C.stModels
      pure $ find (\m -> m.miName == n) models



handleButtonDown :: C.Name -> Vty.Button -> [Vty.Modifier] -> B.Location -> B.EventM C.Name C.UiState ()
handleButtonDown name button ms _loc =
  case (name, button, ms) of
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
          U.setFooterMessage 5 "Copying message to clipboard"
          liftIO . Clip.setClipboard . Txt.unpack $ msg

    _ -> pass


handleChatUpdated :: M.ChatId -> B.EventM C.Name C.UiState ()
handleChatUpdated chatId  = do
  use C.stChatCurrent >>= \case
    Just currentChat | currentChat.chatId == chatId -> do
      changeChatToStoreCurrent

    _ -> do
      pass


handleChatStreamResponseDone :: BCh.BChan C.Command -> M.ChatId -> Maybe M.MessageDetail -> B.EventM C.Name C.UiState ()
handleChatStreamResponseDone _commandChan chatId detail = do
  -- Save to store
  store <- use C.stStore
  liftIO $ store.swStreamDone chatId detail

  -- Update with the final response
  --  Some updates may have been suppressed by the debounce
  --  This also updates the stCurrent's streaming status
  handleChatUpdated chatId


startNewChat :: BCh.BChan C.Command -> Maybe Text -> B.EventM C.Name C.UiState ()
startNewChat commandChan defaultModel = do
  st <- B.get
  store <- use C.stStore
  C.stPopup .= Just C.PopupChatEdit
  C.stPopChatEditTitle .= Just "New chat"
  C.stPopChatEditForm .= BFm.setFormConcat (Wc.vBoxWithPadding 1) (WPce.mkPopChatEditForm M.emptyChatEditInfo
    { M._ceiModels = st._stModels
    , M._ceiSelectedModel = find (\i -> Just i.miName == defaultModel) st._stModels
    })
  C.stPopChatEditOnOk .= \name model prms -> do
    chat <- liftIO $ store.swNewChat M.SsNotStreaming name model.miName prms
    liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat.chatId)



handleChatSelectionUpdate :: B.EventM C.Name C.UiState ()
handleChatSelectionUpdate = do
  saveStoreCurrentChat

  store <- use C.stStore
  selectedChat <- use (C.stChatsList . to BL.listSelectedElement)
  _ <- liftIO $ store.swSetCurrent ((M.chatId) . snd <$> selectedChat)

  changeChatToStoreCurrent


saveStoreCurrentChat :: B.EventM C.Name C.UiState ()
saveStoreCurrentChat = do
  store <- use C.stStore
  liftIO store.swGetCurrent >>= \case
    Nothing -> pass
    Just (_, chat1, _) -> do
      st <- B.get
      let chat2 = chat1
            { M.chatInput = st._stChatInput ^. BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip
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
      liftIO . Txt.appendFile "/home/andre/temp/a.txt" $ Txt.intercalate "\n" $ M.msgText <$> ms

    Nothing -> do
      C.stChatCurrent .= Nothing
      C.stChatInput . BE.editContentsL .= TxtZ.textZipper [] Nothing
      C.stChatMsgs .= []


gotChatsList :: [M.Chat] -> Maybe M.ChatId -> B.EventM C.Name C.UiState ()
gotChatsList chats overrideSelect' = do
  C.stChatsList %= BL.listReplace (V.fromList chats) Nothing

  case overrideSelect' of
    Just chatId -> C.stChatsList %= BL.listFindBy (\i -> i.chatId == chatId)
    Nothing -> C.stChatsList %= BL.listMoveTo 0

  handleChatSelectionUpdate


chatSend :: TV.TVar C.RunState -> C.AppConfig -> BCh.BChan C.UiEvent -> Sr.StoreWrapper -> M.ChatId -> M.ChatMessage -> IO ()
chatSend rs' cfg eventChan store chatId msg = do
  store.swGetChat chatId >>= \case
    Just (chat, hist1) -> do
      debouncedUpdateUi <- Deb.mkDebounce Deb.defaultDebounceSettings
        { Deb.debounceFreq = 500_000  -- 500ms
        , Deb.debounceEdge = Deb.leadingEdge
        , Deb.debounceAction = BCh.writeBChan eventChan $ C.UeChatUpdated chatId
        }

      streamingMessageId <- liftIO $ M.MessageId <$> U.newUuidText

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
                  BCh.writeBChan eventChan $ C.UeChatStreamResponseDone chatId (Just $ M.messageDetailFromChatResponse cr)
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


refreshChatsList :: BCh.BChan C.UiEvent -> Sr.StoreWrapper -> Maybe (Either Text M.ChatId) -> IO ()
refreshChatsList eventChan store overrideSelect1 = do
  chats1 <- store.swListChats
  let chats = reverse $ sortOn (M.chatUpdatedAt) chats1
  let overrideSelect =
       case overrideSelect1 of
         Nothing -> Nothing
         Just (Right chatId) -> Just chatId
         Just (Left chatIdent) -> M.chatId <$> find (\c -> (M.unChatId c.chatId) == chatIdent || c.chatName == chatIdent) chats

  BCh.writeBChan eventChan $ C.UeGotChatsList chats overrideSelect
----------------------------------------------------------------------------------------------------------------------



