{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupHelp
  ( drawPopupHelp
  , handleEventPopupHelp
  ) where

import Verset
import Brick.BChan qualified as BCh
import Brick qualified as B
import Control.Lens ((.=))
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


---------------------------------------------------------------------------------------------------
-- Draw
---------------------------------------------------------------------------------------------------
drawPopupHelp :: C.UiState -> B.Widget C.Name
drawPopupHelp st =
  let _scrollTo = case st._stTab of
        C.TabModels -> Just ("models"::Text)
        C.TabPs -> Just "ps"
        C.TabChat -> Just "chat"
        C.TabColours -> Just "colours"
        C.TabLog -> Just "log"
  in
  B.vLimit 60 $
  B.hLimit 150 $
  Wc.borderWithLabel' True "Help"  $
  B.withAttr (B.attrName "popupHelp") $
  B.padAll 1 $
  B.withClickableVScrollBars C.VScrollClick . B.withVScrollBarHandles . B.withVScrollBars B.OnRight $
  B.viewport C.NHelpScroll B.Vertical $ st._stHelp
---------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupHelp :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupHelp _commandChan _ev ve = do
  case ve of
    Vty.EvKey k ms -> do
      case (k, ms) of
        (Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (Vty.KDown, []) -> do
          B.vScrollBy (B.viewportScroll C.NHelpScroll) 1

        (Vty.KUp, []) -> do
          B.vScrollBy (B.viewportScroll C.NHelpScroll) (-1)

        (Vty.KPageUp, []) -> do
          B.vScrollPage (B.viewportScroll C.NHelpScroll) B.Up

        (Vty.KPageUp, [Vty.MCtrl]) -> do
          B.vScrollToBeginning (B.viewportScroll C.NHelpScroll)

        (Vty.KPageDown, []) -> do
          B.vScrollPage (B.viewportScroll C.NHelpScroll) B.Down

        (Vty.KPageDown, [Vty.MCtrl]) -> do
          B.vScrollToEnd (B.viewportScroll C.NHelpScroll)

        _ -> pass

    _ -> pass
----------------------------------------------------------------------------------------------------------------------



