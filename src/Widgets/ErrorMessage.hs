{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.ErrorMessage
  ( drawErrorMessage
  , handleEventErrorMessage
  ) where

import Verset
import Brick ((<=>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Control.Lens ((.=))
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


---------------------------------------------------------------------------------------------------
-- Draw
---------------------------------------------------------------------------------------------------
drawErrorMessage :: C.UiState -> B.Widget C.Name
drawErrorMessage st =
  B.vLimit 25 $
  B.hLimit 180 $
  borderWithLabel' True "Error" $
  B.withAttr (B.attrName "popupError") $
  B.padAll 1 $
  ( (B.withAttr (B.attrName "popupErrorText") $ B.txtWrap (fromMaybe "??" st._stErrorMessage ))
    <=> B.fill ' '
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
      in
      ( B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Ok"))
      )
    )
  )
---------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventErrorMessage :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventErrorMessage _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopPromptFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stErrorMessage .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stErrorMessage .= Nothing

        (_, Vty.KEnter, []) -> do
          C.stErrorMessage .= Nothing

        _ -> pass

      pass
    _ -> pass
----------------------------------------------------------------------------------------------------------------------



