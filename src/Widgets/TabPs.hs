{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.TabPs
  ( drawTabPs
  , handleTabPs
  , handleAppEventPsList
  , refreshPs
  ) where

import Verset
import Brick ((<=>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.List qualified as BL
import Control.Lens ((^?), (.=), to, use)
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Core qualified as C
import Utils qualified as U
import Widgets.Common as Wc


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawTabPs :: C.UiState -> B.Widget C.Name
drawTabPs st =
  (
    B.vLimit 1 $ B.hBox [
        col 70 "Name" "colHeader"
      , col 11 "Size" "colHeader"
      , col 11 "VRAM" "colHeader"
      , col 40 "Expires" "colHeader"
      ]
  )
  <=>
  if st._stLoadingPs
  then (Wc.spinner st <=> B.fill ' ')
  else
    BL.renderList (\_ p -> renderPsListItem p) (BF.focusGetCurrent st._stFocusPs == Just C.NListPs) st._stPs

  where
    renderPsListItem p =
      let
        age1 = DT.diffUTCTime p.expiresAt st._stNow
        age2 = U.timeSpanToHuman age1
      in
      B.vLimit 1 $ B.hBox [
          colTb col  70 p.modelName ""
        , colTe col  11 (U.bytesToGb p.size_) ""
        , colTe col  11 (U.bytesToGb p.sizeVRam) ""
        , colTe col  40 age2 ""
        ]
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleTabPs
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabPs _commandChan _ev ve focused k ms = do
  case (focused, k, ms) of
    (Just C.NListPs, Vty.KChar 's', []) -> do
      B.gets (^?  C.stPs . BL.listSelectedElementL . to (.modelName)) >>= \case
        Nothing -> pass
        Just name -> do
          st <- B.get
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
            , hostUrl = Just $ fromMaybe C.ollamaDefaultUrl st._stAppConfig.acOllamaUrl
            , responseTimeOut = Nothing
            , options = Nothing
            }

          U.setFooterMessage 10 $ "stopping " <> name

      pass

    (Just C.NListPs, _, _) -> do
      B.zoom C.stPs $ BL.handleListEventVi BL.handleListEvent ve

    _ -> pass

handleAppEventPsList :: BCh.BChan C.Command -> [O.RunningModel] -> B.EventM C.Name C.UiState ()
handleAppEventPsList _commandChan ps' = do
  let ps = sortOn (.modelName) ps'
  psl <- use C.stPs
  let wasSelected = fromMaybe "" $ (.modelName) . snd <$> BL.listSelectedElement psl
  C.stPs .= BL.listFindBy (\i -> i.modelName == wasSelected) (BL.list C.NModelsList (V.fromList ps) 1)
  C.stLoadingPs .= False


refreshPs :: BCh.BChan C.UiEvent -> C.AppConfig -> IO ()
refreshPs eventChan cfg = do
  ps' <- O.psOps (Just $ fromMaybe C.ollamaDefaultUrl cfg.acOllamaUrl)
  let ps = maybe [] (\(O.RunningModels x) -> x) ps'
  BCh.writeBChan eventChan . C.UePsList $ ps
----------------------------------------------------------------------------------------------------------------------



