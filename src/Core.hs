{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Core where

import           Verset

import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Lens (makeLenses)
import Data.Aeson qualified as Ae
import Data.Time qualified as DT
import Ollama qualified as O

data Name
  = NModelsList
  | NModelEditSearch
  | NModelEditTag
  --
  | NListPs
  --
  | NChatInputEdit
  | NChatMsgList
  deriving stock (Show, Eq, Ord)


data UiEvent
  = UeTick !Int
  | UeGotTime !DT.UTCTime
  --
  | UeGotModelList ![O.ModelInfo]
  | UeGotModelShow !(Text, O.ShowModelResponse)
  | UeModelShowDone
  --
  | UePsList ![O.RunningModel]
  --
  | UeChatUpdated !ChatId
  | UeChatStreamResponseDone !ChatId
  deriving stock (Show, Eq)


data Command
  = CmdRefreshModelList
  | CmdRefreshModelShow ![Text]
  --
  | CmdRefreshPs
  --
  | CmdChatSend !ChatId !ChatMessage
  deriving stock (Show, Eq)

newtype ChatId = ChatId Text
  deriving stock (Show, Eq, Ord, Generic)

newtype ChatMessageId = ChatMessageId Text
  deriving stock (Show, Eq, Ord, Generic)

newtype MessageId = MessageId Text
  deriving stock (Show, Eq, Ord, Generic)


data UiState = UiState
  { _stTick :: !Int
  , _stAppConfig :: !AppConfig
  , _stTime :: !DT.UTCTime
  , _stTab :: !Tab
  , _stNow :: !DT.UTCTime
  , _stDebug :: !Text
  , _stStore :: !StoreWrapper
  , _stAttrMap :: !B.AttrMap

  , _stFooterWidget :: !(Maybe (Name, UiState -> B.Widget Name))

  , _stModels :: ![ModelItem]
  , _stModelsList :: !(BL.List Name ModelItem)
  , _stModelsFilter :: !Text
  , _stModelListLoading :: !Bool
  , _stModelShowLoading :: !Bool
  , _stFocusModels :: !(BF.FocusRing Name)
  , _stModelFilterEditor :: !(BE.Editor Text Name)
  , _stModelTagEditor :: !(BE.Editor Text Name)

  , _stPs :: !(BL.List Name O.RunningModel)
  , _stFocusPs :: !(BF.FocusRing Name)
  , _stLoadingPs :: !Bool

  , _stFocusChat :: !(BF.FocusRing Name)
  , _stChatInput :: !(BE.Editor Text Name)
  , _stChatCurrent :: !(Maybe (ChatId, StreamingState))
  , _stChatMsgList :: !(BL.List Name ChatMessage)
  }


data Chat = Chat
  { chatId :: !ChatId
  , chatName :: !Text
  , chatCreatedAt :: !DT.UTCTime
  , chatUpdatedAt :: !DT.UTCTime
  , chatModel :: !Text
  } deriving stock (Show, Eq)

data ChatMessage = ChatMessage
  { msgChatId :: !ChatId
  , msgId :: !MessageId
  , msgRole :: !O.Role
  , msgModel :: !Text
  , msgCreatedAt :: !DT.UTCTime
  , msgText :: !Text
  } deriving stock (Show, Eq)


data Store = Store
  { srListChats :: !(IO [Chat])
  , srLoadChat :: !(ChatId -> IO (Maybe (Chat, [ChatMessage])))
  , srSaveChat :: !(Chat -> IO ())
  , srSaveChatMessage :: !(ChatMessage -> IO ())
  }

data StoreWrapper = StoreWrapper
  { swListChats :: !(IO [Chat])

  , swNewChat :: !(Text -> Text -> IO Chat)
  , swGetChat :: !(ChatId -> IO (Maybe (Chat, [ChatMessage])))
  , swSetCurrent :: !(ChatId -> IO ())
  , swGetCurrent :: !(IO (Maybe (ChatId, Chat, StreamingState, [ChatMessage])))
  , swAddMessage :: !(ChatId -> O.Role -> StreamingState -> Text -> Text -> IO (Either Text ChatMessage))
  , swStreamDone :: !(ChatId -> IO ())
  , swAddStreamedChatContent :: !(ChatId -> MessageId -> O.Role -> Text -> IO (Either Text ()))
  }


data ModelItem = ModelItem
  { miName :: !Text
  , miInfo :: !O.ModelInfo
  , miShow :: !(Maybe O.ShowModelResponse)
  , miTag :: !Text
  }

data Tab
  = TabModels
  | TabPs
  | TabChat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data AppConfig = AppConfig
  { acModelTag :: !(Map Text Text)
  } deriving (Show, Eq, Generic)

data StreamingState
  = SsStreaming
  | SsNotStreaming
  deriving stock (Show, Eq, Ord)


instance Ae.FromJSON AppConfig where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }

instance Ae.ToJSON AppConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }


renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d


ollamaUrl :: Text
ollamaUrl = "http://localhost:11434"
--ollamaUrl = "http://localhost:11435"


makeLenses ''UiState
