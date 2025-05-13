{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupContextMenu
  ( drawPopupContext
  , handleEventPopupContext
  ) where

import Verset
import Brick ((<=>), (<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.List qualified as BL
import Control.Lens ((.=), (%=))
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


---------------------------------------------------------------------------------------------------
-- Draw
---------------------------------------------------------------------------------------------------
drawPopupContext :: C.UiState -> B.Widget C.Name
drawPopupContext st =
  B.vLimit 30 $
  B.hLimit 80 $
  Wc.borderWithLabel' True (fromMaybe "Context Menu" st._stPopContextTitle) $
  B.padAll 1 $
  ( BL.renderList (\_ (_n, v) -> B.txt v) (BF.focusGetCurrent st._stPopContextFocus == Just C.NPopContextList) st._stPopContextList
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          if BF.focusGetCurrent st._stPopContextFocus == Just C.NDialogOk
          then (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonOk", BBS.unicode)

        (attrCancel, attrBorderCancel) =
          if BF.focusGetCurrent st._stPopContextFocus == Just C.NDialogCancel
          then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonCancel", BBS.unicode)
      in
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Yes"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "No"))
      )
    )
  )
---------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupContext :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupContext _commandChan _ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopContextFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopContextFocus %= BF.focusNext

        (_, Vty.KBackTab, []) -> do
          C.stPopContextFocus %= BF.focusPrev

        (Just C.NPopContextList, Vty.KEnter, []) -> do
          ok st

        (Just C.NPopContextList, _, _) -> do
          B.zoom C.stPopContextList $ BL.handleListEventVi BL.handleListEvent ve

        (Just C.NDialogOk, Vty.KEnter, []) ->
          ok st

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass

  where
    ok st = do
      case BL.listSelectedElement st._stPopContextList of
        Nothing -> pass
        Just (_, (n, _)) -> do
          clear
          st._stPopContextOnOk n

    clear = do
      C.stPopup .= Nothing
      C.stPopContextTitle .= Nothing
      C.stPopContextList .= BL.list C.NPopContextList (V.fromList []) 1
      C.stPopContextFocus %= BF.focusSetCurrent C.NPopContextList
      C.stPopContextOnOk .= const pass
----------------------------------------------------------------------------------------------------------------------
