{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.TabLog
  ( drawTabLogs
  , handleTabLog
  , logUpdated
  , updateLog
  ) where

import Verset
import Brick ((<=>))
import Brick.BChan qualified as BCh
import Brick qualified as B
import Brick.Widgets.List qualified as BL
import Control.Lens ((.=), (%=))
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty

import Core qualified as C
import Utils qualified as U
import Widgets.Common (col)


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawTabLogs :: C.UiState -> B.Widget C.Name
drawTabLogs st =
  ( B.vLimit 1 $ B.hBox
      [ col 22 "Date" "colHeader"
      , col 12 "Level" "colHeader"
      , col 9 "Message" "colHeader"
      ]
  )
  <=>
  BL.renderList go True st._stLogList

  where
    go :: Bool -> C.LogEntry -> B.Widget C.Name
    go _ l =
      let dt = Txt.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M:%S" l.leTime
      in
      B.hBox
        [ col 22 dt ""
        , col 12 (show l.leLevel) ""
        , B.txtWrap l.leText
        ]
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleTabLog
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabLog _commandChan _ev ve _focused k ms = do
  case (k, ms) of
    (Vty.KChar '+', []) -> do
      --let fs = length U.logLevelFilters
      pass

    (Vty.KChar '-', []) -> do
      --let fs = length U.logLevelFilters
      pass

    _ -> do
      --C.stDebug .= show (k, ms)
      B.zoom C.stLogList $ BL.handleListEventVi BL.handleListEvent ve



logUpdated :: [C.LogEntry] -> (C.LogLevel, Text) -> B.EventM C.Name C.UiState ()
logUpdated ls (lvl, msg) = do
  C.stLogList %= BL.listReplace (V.fromList ls) Nothing
  C.stLogList %= BL.listMoveToEnd

  when (lvl `elem` [C.LlCritical, C.LlError, C.LlWarn]) $ do
    C.stDebug .= (U.logLevelName lvl) <> ":" <> msg

  when (lvl `elem` [C.LlCritical, C.LlError]) $ do
    C.stErrorMessage .= Just msg



updateLog :: BCh.BChan C.UiEvent -> C.StoreWrapper -> C.LogLevel -> Text -> IO ()
updateLog eventChan store l e = do
  ls <- store.swLog.lgReadLast 1000
  BCh.writeBChan eventChan $ C.UeLogUpdated ls (l, e)
----------------------------------------------------------------------------------------------------------------------
