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
  | NChatsList
  --
  | NColoursList
  --
  | NDialogOk
  | NDialogCancel
  --
  | NPopChatEditName
  | NPopChatEditModels
  --
  | NLogList
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
  | UeGotChatsList ![Chat] !(Maybe ChatId)
  | UeChatUpdated !ChatId
  | UeChatStreamResponseDone !ChatId
  --
  | UeLogUpdated ![LogEntry] !(LogLevel, Text)
  deriving stock (Show, Eq)


data Command
  = CmdRefreshModelList
  | CmdRefreshModelShow ![Text]
  --
  | CmdRefreshPs
  --
  | CmdRefreshChatsList !(Maybe ChatId)
  | CmdChatSend !ChatId !ChatMessage
  --
  | CmdUpdateLog !LogLevel !Text
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
  , _stLog :: !Logger
  , _stAttrMap :: !B.AttrMap
  , _stErrorMessage :: !(Maybe Text)

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
  , _stChatCurrent :: !(Maybe (Chat, StreamingState))
  , _stChatMsgList :: !(BL.List Name ChatMessage)
  , _stChatsList :: !(BL.List Name Chat)

  , _stColoursList :: !(BL.List Name Text)

  , _stFocusLog :: !(BF.FocusRing Name)
  , _stLogList :: !(BL.List Name LogEntry)
  , _stLogFilterIndex :: !Int

  , _stPopup :: !(Maybe Popup)

  , _stPopChatEditFocus :: !(BF.FocusRing Name)
  , _stPopChatEditTitle :: !(Maybe Text)
  , _stPopChatEditName :: !(BE.Editor Text Name)
  , _stPopChatEditModels :: !(BL.List Name ModelItem)
  , _stPopChatEditOnOk :: !(Text -> Text -> B.EventM Name UiState ())
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

  , swNewChat :: !(Text -> Text -> StreamingState -> IO Chat)
  , swGetChat :: !(ChatId -> IO (Maybe (Chat, [ChatMessage], StreamingState)))
  , swSaveChat :: !(Chat -> IO ())
  , swSetCurrent :: !(Maybe ChatId -> IO (Maybe (Chat, [ChatMessage], StreamingState)))
  , swGetCurrent :: !(IO (Maybe (ChatId, Chat, StreamingState, [ChatMessage])))
  , swAddMessage :: !(ChatId -> O.Role -> StreamingState -> Text -> Text -> IO (Either Text ChatMessage))
  , swStreamDone :: !(ChatId -> IO ())
  , swAddStreamedChatContent :: !(ChatId -> MessageId -> O.Role -> Text -> IO (Either Text ()))

  , swLog :: !Logger
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
  | TabColours
  | TabLog
  deriving stock (Show, Eq, Ord, Enum, Bounded)


data Popup
  = PopupChatEdit
  deriving stock (Show, Eq, Ord, Bounded, Enum)


data AppConfig = AppConfig
  { acModelTag :: !(Map Text Text)
  , acDefaultModel :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

data StreamingState
  = SsStreaming
  | SsNotStreaming
  deriving stock (Show, Eq, Ord)


data LogLevel
  = LlDebug
  | LlInfo
  | LlWarn
  | LlError
  | LlCritical
  deriving stock (Show, Eq, Ord, Bounded, Enum)


data Logger = Logger
  { lgWarn :: !(Text -> IO ())
  , lgError :: !(Text -> IO ())
  , lgInfo :: !(Text -> IO ())
  , lgDebug :: !(Text -> IO ())
  , lgCritical :: !(Text -> IO ())
  , lgReadLast :: !(Int -> IO [LogEntry])
  }

data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leTime :: !DT.UTCTime
  , leText :: !Text
  } deriving stock (Show, Eq)


instance Ae.FromJSON AppConfig where
  parseJSON = Ae.withObject "AppConfig" $ \o -> do
    acModelTag <- o Ae..:? "model_tag"
    acDefaultModel <- o Ae..:? "default_model"
    pure $ AppConfig (fromMaybe mempty acModelTag) acDefaultModel

instance Ae.ToJSON AppConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }


renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d


ollamaUrl :: Text
ollamaUrl = "http://localhost:11434"
--ollamaUrl = "http://localhost:11435"


makeLenses ''UiState
