{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Storage
  ( newInMemStore
  , newStoreWrapper
  ) where

import           Verset

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Control.Concurrent.MVar.Strict (MVar', newMVar', modifyMVar', modifyMVar'_, withMVar')
import Data.Time qualified as DT
import Data.Map.Strict qualified as Map
import Ollama qualified as O

import Core qualified as C
import Utils qualified as U


newInMemStore :: IO C.Store
newInMemStore = do
  store :: TV.TVar (Map C.ChatId (C.Chat, [C.ChatMessage])) <- atomically $ TV.newTVar Map.empty

  pure $ C.Store
    { srListChats = do
        chats <- atomically $ Map.toList <$> TV.readTVar store
        pure $ (\(k, (c, _)) -> c { C.chatId = k }) <$> chats

    , srLoadChat = \chatId -> do
        chats <- atomically $ TV.readTVar store
        pure $ Map.lookup chatId chats

    , srSaveChat = \chat -> do
        atomically $ TV.modifyTVar' store (Map.insert chat.chatId (chat, []))

    , srSaveChatMessage = \msg -> do
        atomically $ TV.modifyTVar' store $ \m ->
          case Map.lookup msg.msgChatId m of
            Nothing -> m
            Just (c, msgs) ->
              Map.insert msg.msgChatId (c, msg : msgs) m
    }


data WrapperState = WrapperState
  { store :: !C.Store
  , cache :: !(Map C.ChatId (Bool, C.Chat, [(C.ChatMessage, Maybe C.StreamId)]))
  , currentId :: !(Maybe C.ChatId)
  }


newStoreWrapper :: (IO C.Store) -> IO C.StoreWrapper
newStoreWrapper mkStore = do
  storeRaw <- mkStore

  -- Create a new MVar to hold the state
  --  This ensures that the state is thread-safe and can be modified safely
  --  this includes the cache and access to the underlying store
  --  So we should never use the storeRaw directly
  st <- newMVar' $ WrapperState
    { store = storeRaw
    , cache = Map.empty
    , currentId = Nothing
    }

  --TODO timer to call evict

  pure $ C.StoreWrapper
    { swListChats = listChats st

    , swNewChat = newChat st
    , swGetChat = getChat st
    , swSetCurrent = \c -> setCurrent st c >>= evict st
    , swGetCurrent = getCurrent st
    , swAddMessage = addMessage st
    , swStreamDone = \c s -> streamDone st c s >>= evict st
    , swAddStreamedChatContent = addStreamedChatContent st
    }

  where
    listChats :: MVar' WrapperState -> IO [C.Chat]
    listChats st' =
      withMVar' st' $ \st -> st.store.srListChats


    evict :: MVar' WrapperState -> a -> IO a
    evict st' a = do
      modifyMVar'_ st' $ \st -> do
        let vs2 = Map.filterWithKey (\k (isStreaming, _, _) -> isStreaming || Just k /= st.currentId) st.cache
        pure $ st { cache = vs2 }
      pure a


    newChat :: MVar' WrapperState -> Text -> Text -> IO C.Chat
    newChat st' chatName chatModel = do
      chatId <- C.ChatId <$> U.newUuidText
      now <- DT.getCurrentTime

      let chat = C.Chat
            { C.chatId = chatId
            , C.chatName = chatName
            , C.chatCreatedAt = now
            , C.chatUpdatedAt = now
            , C.chatModel = chatModel
            }

      modifyMVar'_ st' $ \st -> do
        let cache2 = Map.insert chatId (False, chat, []) st.cache
        _ <- st.store.srSaveChat chat
        pure st { cache = cache2 }

      pure chat


    getChat :: MVar' WrapperState -> C.ChatId -> IO (Maybe (C.Chat, [C.ChatMessage]))
    getChat st' chatId = do
      withMVar' st' $ \st -> do
        case Map.lookup chatId st.cache of
          Nothing -> st.store.srLoadChat chatId
          Just (_isStreaming, chat, ms) -> pure $ Just (chat, fst <$> ms)


    setCurrent :: MVar' WrapperState -> C.ChatId -> IO ()
    setCurrent st' chatId = do
      modifyMVar'_ st' $ \st -> pure st { currentId = Just chatId }


    getCurrent :: MVar' WrapperState -> IO (Maybe (C.ChatId, C.Chat, [C.ChatMessage]))
    getCurrent st' = do
      modifyMVar' st' $ \st -> do
        case st.currentId of
          -- No current chat
          Nothing -> pure (st, Nothing)
          -- Current chat is set
          Just currentId -> do
            case Map.lookup currentId st.cache of
              -- Current chat is in cache
              Just (_isStreaming, chat, ms) ->
                pure (st, Just (currentId, chat, fst <$> ms))

              -- Current chat is not in cache
              Nothing -> do
                st.store.srLoadChat currentId >>= \case
                  -- Current chat is not in store
                  Nothing ->
                    pure (st, Nothing)

                  -- Current chat is in store
                  Just (c, ms) -> do
                    -- Update cache with the loaded chat
                    let st2 = st { cache = Map.insert currentId (False, c, (,Nothing) <$> ms) st.cache }
                    -- done
                    pure (st2, Just (currentId, c, ms))




    addMessage :: MVar' WrapperState -> C.ChatId -> O.Role -> Bool -> Text -> Text -> IO (Either Text C.ChatMessage)
    addMessage st' chatId role isStreaming model text = do
      now <- DT.getCurrentTime
      mid <- U.newUuidText
      let newMsg = C.ChatMessage
            { C.msgChatId = chatId
            , C.msgId = mid
            , C.msgRole = role
            , C.msgModel = model
            , C.msgCreatedAt = now
            , C.msgText = text
            }

      modifyMVar'_ st' $ \st -> do
        -- Update the cache with the new message, if it is in the cache
        let cache2 =
             Map.adjust
              (\(_, chat, ms1) ->
                let ms2 = (newMsg, Nothing) : ms1 in
                (isStreaming, chat, ms2)
                )
              chatId
              st.cache

        -- Save the new message to the store
        _ <- st.store.srSaveChatMessage newMsg
        pure st { cache = cache2 }

      pure . Right $ newMsg


    streamDone :: MVar' WrapperState -> C.ChatId -> C.StreamId -> IO ()
    streamDone st' chatId streamId = do
      pass --TODO


    addStreamedChatContent :: MVar' WrapperState -> C.ChatId -> C.StreamId -> O.Role -> Text -> IO (Either Text ())
    addStreamedChatContent st' chatId streamId role text = do
      modifyMVar' st' $ \st -> do
        current' <-
          -- Get from cache or store
          case Map.lookup chatId st.cache of
            Just (_, chat, ms) -> pure $ Just (chat, ms)
            Nothing -> do
              st.store.srLoadChat chatId >>= \case
                Nothing -> pure Nothing
                Just (chat, ms) -> do
                  let ms2 = (,Nothing) <$> ms
                  pure $ Just (chat, ms2)

        case current' of
          Nothing -> pure (st, Left $ "Chat not found in cache or store: " <> show chatId)
          Just (chat, ms1) -> do
            (current1, rest) <-
              case ms1 of
                ((m, Just sid) : rest') | sid == streamId -> pure (m, rest')
                _ -> do
                  msgId <- U.newUuidText
                  now <- DT.getCurrentTime
                  pure ( C.ChatMessage
                           { C.msgChatId = chatId
                           , C.msgId = msgId
                           , C.msgRole = role
                           , C.msgModel = ""
                           , C.msgCreatedAt = now
                           , C.msgText = ""
                           }
                       , ms1)

            let current2 = current1 {C.msgText = C.msgText current1 <> text}
            let st2 = st { cache = Map.insert chatId (True, chat, (current2, Just streamId) : rest) st.cache }
            pure (st2, Right ())

