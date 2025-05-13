{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupPrompt
  ( drawPopupPrompt
  , handleEventPopupPrompt
  ) where

import Verset
import Brick ((<=>), (<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.Edit qualified as BE
import Control.Lens ((%=), (.=), use, to)
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawPopupPrompt :: C.UiState -> B.Widget C.Name
drawPopupPrompt st =
  B.vLimit 10 $
  B.hLimit 180 $
  Wc.borderWithLabel' True (fromMaybe "Enter text" st._stPopPromptTitle) $
  B.withAttr (B.attrName "popup") $
  B.padAll 1 $
  ( ( B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent st._stPopPromptFocus == Just C.NPopPromptEdit) st._stPopPromptEdit
    )
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          if BF.focusGetCurrent st._stPopPromptFocus == Just C.NDialogOk
          then (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonOk", BBS.unicode)

        (attrCancel, attrBorderCancel) =
          if BF.focusGetCurrent st._stPopPromptFocus == Just C.NDialogCancel
          then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonCancel", BBS.unicode)
      in
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Ok"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Cancel"))
      )
    )
  )
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupPrompt :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupPrompt _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopPromptFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        (_, Vty.KChar '\t', []) -> do
          C.stPopPromptFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopPromptFocus %= BF.focusPrev

        (Just C.NPopPromptEdit, Vty.KChar 'k', [Vty.MCtrl]) -> do
          C.stPopPromptEdit . BE.editContentsL %= TxtZ.clearZipper

        (Just C.NPopPromptEdit, Vty.KEnter, []) -> do
          accept st

        (Just C.NPopPromptEdit, _, _) -> do
          B.zoom C.stPopPromptEdit $ BE.handleEditorEvent ev

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          accept st

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          C.stPopup .= Nothing
          C.stPopPromptTitle .= Nothing
          C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit

        _ -> pass

    _ -> pass

  where
    accept st = do
      txt <- use (C.stPopPromptEdit . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)
      C.stPopup .= Nothing
      C.stPopPromptTitle .= Nothing
      C.stPopPromptFocus %= BF.focusSetCurrent C.NPopPromptEdit
      st._stPopPromptOnOk txt
----------------------------------------------------------------------------------------------------------------------



