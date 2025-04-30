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
  deriving stock (Show, Eq)


data Command
  = CmdRefreshModelList
  | CmdRefreshModelShow ![Text]
  --
  | CmdRefreshPs
  deriving stock (Show, Eq, Ord)


data UiState = UiState
  { _stTick :: !Int
  , _stAppConfig :: !AppConfig
  , _stTime :: !DT.UTCTime
  , _stTab :: !Tab
  , _stNow :: !DT.UTCTime
  , _stDebug :: !Text

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



instance Ae.FromJSON AppConfig where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }

instance Ae.ToJSON AppConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }


renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d


ollamaUrl :: Text
ollamaUrl = "http://localhost:11434"


makeLenses ''UiState
