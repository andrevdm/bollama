{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupConfirm
  ( drawPopupConfirm
  , handleEventPopupConfirm
  ) where

import Verset
import Brick ((<=>), (<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Control.Lens ((%=), (.=))
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawPopupConfirm :: C.UiState -> B.Widget C.Name
drawPopupConfirm st =
  B.vLimit 14 $
  B.hLimit 150 $
  Wc.borderWithLabel' True (fromMaybe "Confirm" st._stPopConfirmTitle) $
  B.withAttr (B.attrName "popup") $
  B.padAll 1 $
  ( ( B.withAttr (B.attrName "popupHeader") $
      B.txt (fromMaybe "Are you sure?" st._stPopConfirmTitle)
    )
    <=>
    ( B.withAttr (B.attrName "popupHeader") $
      B.txtWrap (fromMaybe "" st._stPopConfirmDetail)
    )
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          if BF.focusGetCurrent st._stPopConfirmFocus == Just C.NDialogOk
          then (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonOk", BBS.unicode)

        (attrCancel, attrBorderCancel) =
          if BF.focusGetCurrent st._stPopConfirmFocus == Just C.NDialogCancel
          then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonCancel", BBS.unicode)
      in
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Yes"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "No"))
      )
    )
  )
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupConfirm :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupConfirm _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopConfirmFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          clear

        (_, Vty.KEsc, []) -> do
          clear

        (_, Vty.KChar '\t', []) -> do
          C.stPopConfirmFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopConfirmFocus %= BF.focusPrev

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          _ <- st._stPopConfirmOnOk
          clear

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass
  where
    clear = do
      C.stPopup .= Nothing
      C.stPopConfirmTitle .= Nothing
      C.stPopConfirmDetail .= Nothing
      C.stPopConfirmFocus %= BF.focusSetCurrent C.NDialogCancel
----------------------------------------------------------------------------------------------------------------------



