{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

module Core where

import Verset
import Brick.Focus qualified as BF
import Brick.Forms qualified as BFm
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.FileBrowser qualified as BFi
import Brick.Widgets.List qualified as BL
import Control.Concurrent.STM.TVar qualified as TV
import Control.Lens (makeLenses)
import Data.Aeson qualified as Ae
import Data.Time qualified as DT
import Ollama qualified as O

import Logging qualified as L
import Messages qualified as M
import Storage.Store qualified as Sr

data Name
  = NModelsList
  | NModelEditSearch
  | NModelEditTag
  --
  | NListPs
  --
  | NChatInputEdit
  | NChatsList
  | NChatScroll
  | NChatMsgCopy !M.MessageId
  --
  | NColoursList
  --
  | NDialogOk
  | NDialogCancel
  --
  | NPopChatEditFormName
  | NPopChatEditFormModels
  | NPopChatEditFormCtx
  | NPopChatEditFormTemp
  --
  | NPopPromptEdit
  --
  | NLogList
  --
  | VScrollClick B.ClickableScrollbarElement Name
  --
  | NHelpScroll
  --
  | NPopContextList
  --
  | NPopExportBrowser
  | NPopExportDir
  | NPopExportFileName
  | NPopExportFormatJson
  | NPopExportFormatText
  deriving stock (Show, Eq, Ord)


data UiEvent
  = UeInit
  | UeTick !Int
  | UeGotTime !DT.UTCTime
  --
  | UeGotModelList ![O.ModelInfo]
  | UeGotModelShow !(Text, O.ShowModelResponse)
  | UeModelShowDone
  --
  | UePsList ![O.RunningModel]
  --
  | UeGotChatsList ![M.Chat] !(Maybe M.ChatId)
  | UeChatUpdated !M.ChatId
  | UeChatStreamResponseDone !M.ChatId !(Maybe M.MessageDetail)
  --
  | UeLogUpdated ![L.LogEntry] !(L.LogLevel, Text)
  deriving stock (Show, Eq)


data Command
  = CmdRefreshModelList
  | CmdRefreshModelShow ![Text]
  --
  | CmdRefreshPs
  --
  | CmdRefreshChatsList !(Maybe (Either Text M.ChatId))
  | CmdChatSend !M.ChatId !M.ChatMessage
  --
  | CmdUpdateLog !L.LogLevel !Text
  deriving stock (Show, Eq)


data UiState = UiState
  { _stTick :: !Int
  , _stAppConfig :: !AppConfig
  , _stTab :: !Tab
  , _stNow :: !DT.UTCTime
  , _stFooterMessage :: !(Maybe (UTCTime, Text))
  , _stStore :: !Sr.StoreWrapper
  , _stLog :: !L.Logger
  , _stAttrMap :: !B.AttrMap
  , _stErrorMessage :: !(Maybe Text)
  , _stShowThinking :: !Bool
  , _stShowMessageDetail :: !Bool
  , _stRunState :: !(TV.TVar RunState)
  , _stHelp :: !(B.Widget Name)

  , _stModels :: ![M.ModelItem]
  , _stModelsList :: !(BL.List Name M.ModelItem)
  , _stModelsFilter :: !Text
  , _stModelListLoading :: !Bool
  , _stModelShowLoading :: !Bool
  , _stFocusModels :: !(BF.FocusRing Name)

  , _stPs :: !(BL.List Name O.RunningModel)
  , _stFocusPs :: !(BF.FocusRing Name)
  , _stLoadingPs :: !Bool

  , _stFocusChat :: !(BF.FocusRing Name)
  , _stChatInput :: !(BE.Editor Text Name)
  , _stChatCurrent :: !(Maybe M.Chat)
  , _stChatMsgs :: ![M.ChatMessage]
  , _stChatsList :: !(BL.List Name M.Chat)

  , _stColoursList :: !(BL.List Name Text)

  , _stFocusLog :: !(BF.FocusRing Name)
  , _stLogList :: !(BL.List Name L.LogEntry)
  , _stLogFilterIndex :: !Int

  , _stPopup :: !(Maybe Popup)

  , _stPopChatEditFocus :: !(BF.FocusRing Name)
  , _stPopChatEditTitle :: !(Maybe Text)
  , _stPopChatEditOnOk :: !(Text -> M.ModelItem -> M.ChatParams -> B.EventM Name UiState ())
  , _stPopChatEditForm :: !(BFm.Form M.ChatEditInfo UiEvent Name)

  , _stPopPromptFocus :: !(BF.FocusRing Name)
  , _stPopPromptTitle :: !(Maybe Text)
  , _stPopPromptEdit :: !(BE.Editor Text Name)
  , _stPopPromptOnOk :: !(Text -> B.EventM Name UiState ())

  , _stPopConfirmFocus :: !(BF.FocusRing Name)
  , _stPopConfirmTitle :: !(Maybe Text)
  , _stPopConfirmDetail :: !(Maybe Text)
  , _stPopConfirmOnOk :: !(B.EventM Name UiState ())

  , _stPopContextFocus :: !(BF.FocusRing Name)
  , _stPopContextTitle :: !(Maybe Text)
  , _stPopContextList :: !(BL.List Name (Text, Text, Text, Text))
  , _stPopContextOnOk :: !(Text -> B.EventM Name UiState ())

  , _stPopExportFocus :: !(BF.FocusRing Name)
  , _stPopExportOnOk :: !(M.ExportFormat -> FilePath -> B.EventM Name UiState ())
  , _stPopExportBrowser :: !(BFi.FileBrowser Name)
  , _stPopExportDir :: !(BE.Editor Text Name)
  , _stPopExportFName :: !(BE.Editor Text Name)
  , _stPopExportError :: !(Maybe Text)
  , _stPopExportFormat :: !M.ExportFormat
  }

data AppConfig = AppConfig
  { acModelTag :: !(Map Text Text)
  , acDefaultModel :: !(Maybe Text)
  , acDefaultChat :: !(Maybe Text)
  , acAvoidEmojis :: !Bool
  , acDefaultTab :: !Tab
  , acOllamaUrl :: !(Maybe Text)
  , acAllowMouse :: !Bool
  , acDefaultExportDir :: !(Maybe FilePath)
  } deriving (Show, Eq, Generic)


data RunState = RunState
  { rsKilledChats :: !(Set M.MessageId)
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
  | PopupPrompt
  | PopupConfirm
  | PopupHelp
  | PopupContext
  | PopupExport
  deriving stock (Show, Eq, Ord, Bounded, Enum)



instance Ae.FromJSON AppConfig where
  parseJSON = Ae.withObject "AppConfig" $ \o -> do
    acModelTag <- o Ae..:? "model_tag"
    acDefaultModel <- o Ae..:? "default_model"
    acDefaultChatName <- o Ae..:? "default_chat"
    acAvoidEmojis <- o Ae..:? "avoid_emojis"
    acDefaultTab <- o Ae..:? "default_tab" >>= \case
      Just ("models"::Text) -> pure TabModels
      Just "ps" -> pure TabPs
      Just "chat" -> pure TabChat
      Just "colours" -> pure TabColours
      Just "log" -> pure TabLog
      _ -> pure TabModels
    acOllamaUrl <- o Ae..:? "ollama_url"
    acAllowMouse <- o Ae..:? "allow_mouse"
    acDefaultExportDir <- o Ae..:? "default_export_dir"

    pure $ AppConfig (fromMaybe mempty acModelTag) acDefaultModel acDefaultChatName (fromMaybe False acAvoidEmojis) acDefaultTab acOllamaUrl (fromMaybe True acAllowMouse) acDefaultExportDir


instance Ae.ToJSON AppConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }

instance Ae.ToJSON Tab where
  toJSON TabModels = "models"
  toJSON TabPs = "ps"
  toJSON TabChat = "chat"
  toJSON TabColours = "colours"
  toJSON TabLog = "log"



renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d


ollamaDefaultUrl :: Text
ollamaDefaultUrl = "http://localhost:11434"


makeLenses ''UiState
