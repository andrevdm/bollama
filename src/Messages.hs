{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}

module Messages where

import Verset
import Control.Lens (makeLenses)
import Data.Aeson.Encode.Pretty qualified as Ae
import Data.Aeson qualified as Ae
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding qualified as TxtE
import Data.Text.IO qualified as Txt
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Ollama qualified as O


newtype ChatId = ChatId Text
  deriving stock (Show, Eq, Ord, Generic)

newtype MessageId = MessageId Text
  deriving stock (Show, Eq, Ord, Generic)

unChatId :: ChatId -> Text
unChatId (ChatId t) = t

unMessageId :: MessageId -> Text
unMessageId (MessageId t) = t

data Chat = Chat
  { chatId :: !ChatId
  , chatName :: !Text
  , chatCreatedAt :: !DT.UTCTime
  , chatUpdatedAt :: !DT.UTCTime
  , chatModel :: !Text
  , chatStreaming :: !StreamingState
  , chatParams :: !ChatParams
  , chatInput :: !Text
  } deriving stock (Show, Eq)

data ChatParams = ChatParams
  { _cpTemp :: !(Maybe Double)
  , _cpContextSize :: !(Maybe Int)
  } deriving stock (Show, Eq)

data ChatMessage = ChatMessage
  { msgChatId :: !ChatId
  , msgId :: !MessageId
  , msgRole :: !O.Role
  , msgModel :: !Text
  , msgCreatedAt :: !DT.UTCTime
  , msgText :: !Text
  , msgDetail :: !(Maybe MessageDetail)
  } deriving stock (Show, Eq)

data MessageDetail = MessageDetail
  { cdTotalDuration :: !(Maybe Int)
  , cdLoadDuration :: !(Maybe Int)
  , cdPromptEvalCount :: !(Maybe Int)
  , cdPromptEvalDuration :: !(Maybe Int)
  , cdEvalCount :: !(Maybe Int)
  , cdEvalDuration :: !(Maybe Int)
  } deriving stock (Show, Eq)

data ModelItem = ModelItem
  { miName :: !Text
  , miInfo :: !O.ModelInfo
  , miShow :: !(Maybe O.ShowModelResponse)
  , miTag :: !Text
  } deriving stock (Show, Eq)

data ChatEditInfo = ChatEditInfo
  { _ceiName :: !Text
  , _ceiModels :: ![ModelItem]
  , _ceiSelectedModel :: !(Maybe ModelItem)
  , _ceiParams :: !ChatParams
  } deriving (Show, Eq)

data StreamingState
  = SsStreaming
  | SsNotStreaming
  deriving stock (Show, Eq, Ord)

data ExportFormat
  = ExportJson
  | ExportText
  deriving stock (Show, Eq, Ord, Bounded, Enum)


emptyChatEditInfo :: ChatEditInfo
emptyChatEditInfo =
  ChatEditInfo
    { _ceiParams = emptyChatParams
    , _ceiModels = []
    , _ceiSelectedModel = Nothing
    , _ceiName = ""
    }

emptyChatParams :: ChatParams
emptyChatParams =
  ChatParams
    { _cpTemp = Nothing
    , _cpContextSize = Nothing
    }



messageDetailFromChatResponse :: O.ChatResponse -> MessageDetail
messageDetailFromChatResponse cr =
  MessageDetail
    { cdTotalDuration = fromIntegral <$> cr.totalDuration
    , cdLoadDuration = fromIntegral <$> cr.loadDuration
    , cdPromptEvalCount = fromIntegral <$> cr.promptEvalCount
    , cdPromptEvalDuration = fromIntegral <$> cr.promptEvalDuration
    , cdEvalCount = fromIntegral <$> cr.evalCount
    , cdEvalDuration = fromIntegral <$> cr.evalDuration
    }


exportChatToFile :: Chat -> [ChatMessage] -> ExportFormat -> FilePath -> IO ()
exportChatToFile chat ms fmt file = do
  Txt.writeFile file $ exportChat chat ms fmt


exportChat :: Chat -> [ChatMessage]  -> ExportFormat -> Text
exportChat c ms ExportJson = exportChatJson c ms
exportChat c ms ExportText = exportChatText c ms


exportChatJson :: Chat -> [ChatMessage] -> Text
exportChatJson c ms =
  let
    chat = Ae.object
      [ ("id", Ae.toJSON $ unChatId c.chatId)
      , ("name", Ae.toJSON c.chatName)
      , ("createdAt", Ae.toJSON c.chatCreatedAt)
      ]

    chatMessages = ms <&> \m ->
      Ae.object
        [ ("id", Ae.toJSON $ unMessageId m.msgId)
        , ("role", Ae.toJSON m.msgRole)
        , ("model", Ae.toJSON m.msgModel)
        , ("createdAt", Ae.toJSON m.msgCreatedAt)
        , ("text", Ae.toJSON m.msgText)
        ]

    export = Ae.object
      [ ("chat", chat)
      , ("messages", Ae.toJSON chatMessages)
      ]
  in
  TxtE.decodeUtf8 . BSL.toStrict . Ae.encodePretty $ export


exportChatText :: Chat -> [ChatMessage] -> Text
exportChatText c ms = Txt.unlines . concat $
  [
    [ "***************************************************************************************************************************"
    , "# Name: " <> c.chatName
    , " - ID: " <> show (unChatId c.chatId)
    , " - Created: " <> show c.chatCreatedAt
    , "***************************************************************************************************************************"
    , ""
    , ""
    , ""
    ]
  ]
  <>
  ( ms <&> \m ->
      [ "--------------------------------------------------------------------------------------------------------------"
      , "# Role: " <> show m.msgRole
      , " - ID: " <> show (unMessageId m.msgId)
      , " - Model: " <> show m.msgModel
      , " - Created: " <> show m.msgCreatedAt
      , "--------------------------------------------------------------------------------------------------------------"
      , ""
      , m.msgText
      , ""
      , "--------------------------------------------------------------------------------------------------------------"
      , ""
      , ""
      , ""
      ]
  )

makeLenses ''ChatEditInfo
makeLenses ''ChatParams
