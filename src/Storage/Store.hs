{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

module Storage.Store where

import Verset
import Ollama qualified as O

import Logging qualified as L
import Messages qualified as M


data Store = Store
  { srListChats :: !(IO [M.Chat])
  , srLoadChat :: !(M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage])))
  , srSaveChat :: !(M.Chat -> IO ())
  , srSaveChatMessage :: !(M.ChatMessage -> IO ())
  , srClearChatInput :: !(M.ChatId -> IO ())
  , srDeleteAllChatMessages :: !(M.ChatId -> IO ())
  , srDeleteChat :: !(M.ChatId -> IO ())
  , srGetMessageText :: !(M.MessageId -> IO (Maybe Text))
  }

data StoreWrapper = StoreWrapper
  { swListChats :: !(IO [M.Chat])
  , swNewChat :: !(M.StreamingState -> Text -> Text -> M.ChatParams -> IO M.Chat)
  , swGetChat :: !(M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage])))
  , swSaveChat :: !(M.Chat -> IO ())
  , swSetCurrent :: !(Maybe M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage])))
  , swGetCurrent :: !(IO (Maybe (M.ChatId, M.Chat, [M.ChatMessage])))
  , swAddMessage :: !(M.ChatId -> O.Role -> M.StreamingState -> Text -> Text -> IO (Either Text M.ChatMessage))
  , swStreamDone :: !(M.ChatId -> Maybe M.MessageDetail -> IO ())
  , swAddStreamedChatContent :: !(M.ChatId -> M.MessageId -> O.Role -> Text -> IO (Either Text ()))
  , swClearChatInput :: !(M.ChatId -> IO (Either Text ()))
  , swDeleteAllChatMessages :: !(M.ChatId -> IO ())
  , swDeleteChat :: !(M.ChatId -> IO ())
  , swGetMessageText :: !(M.MessageId -> IO (Maybe Text))

  , swLog :: !L.Logger
  }

