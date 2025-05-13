{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Draw
  ( drawUI
  )
  where

import Verset
import Brick ((<=>), (<+>))
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.Core qualified as BW

import Config qualified as Cfg
import Core qualified as C
import Widgets.Common qualified as Wc
import Widgets.ErrorMessage qualified as Wer
import Widgets.PopupConfirm qualified as WPcm
import Widgets.PopupContextMenu qualified as WPctx
import Widgets.PopupEditChat qualified as WPce
import Widgets.PopupExport qualified as WPex
import Widgets.PopupHelp qualified as WPhlp
import Widgets.PopupPrompt qualified as WPpt
import Widgets.TabChat qualified as WTct
import Widgets.TabColours qualified as WTcl
import Widgets.TabLog qualified as WTlg
import Widgets.TabModels qualified as WTmo
import Widgets.TabPs qualified as WTps



---------------------------------------------------------------------------------------------------
-- Main draw function
---------------------------------------------------------------------------------------------------
drawUI :: C.UiState -> [B.Widget C.Name]
drawUI st =
  [ case st._stErrorMessage of
      -- Error message wins
      Just _ -> BC.centerLayer $ Wer.drawErrorMessage st
      -- Otherwise draw current popup if there is one
      Nothing ->
        BC.centerLayer $
          case st._stPopup of
            Nothing -> BW.emptyWidget
            Just C.PopupChatEdit -> WPce.drawPopupChatEdit st
            Just C.PopupPrompt -> WPpt.drawPopupPrompt st
            Just C.PopupConfirm -> WPcm.drawPopupConfirm st
            Just C.PopupHelp -> WPhlp.drawPopupHelp st
            Just C.PopupContext -> WPctx.drawPopupContext st
            Just C.PopupExport -> WPex.drawPopupExport st

  -- Draw the main UI
  , drawTabs st
    <=> footer st
  ]
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Footer
---------------------------------------------------------------------------------------------------
footer :: C.UiState -> B.Widget C.Name
footer st =
  B.vLimit 1 . B.withAttr (B.attrName "footer") $ B.txt (Cfg.verText <> " | ") <+> (B.txt st._stDebug) <+> B.fill ' '
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Tabs
---------------------------------------------------------------------------------------------------
drawTabs :: C.UiState -> B.Widget C.Name
drawTabs st =
  drawTabHeader
  <=>
  (B.padLeft (B.Pad 2) . B.padRight (B.Pad 2) . B.withBorderStyle BBS.unicode . BB.border  $ drawTabsContent)

  where
    drawTabHeader =
      let ts :: [C.Tab] = [minBound .. maxBound] in
      B.padLeft (B.Pad 2) . B.vLimit 1 $
        (B.hBox $ ts <&> \t ->
          B.padRight (B.Pad 2) $
            let
              name = Wc.tabName t
              isSelected = st._stTab == t
              attr = if isSelected then "tabSelected" else "tabUnselected"
            in
            B.withAttr (B.attrName attr) $ B.txt name
        )

    drawTabsContent =
      case st._stTab of
        C.TabModels -> WTmo.drawTabModels st
        C.TabPs -> WTps.drawTabPs st
        C.TabChat -> WTct.drawTabChat st
        C.TabColours -> WTcl.drawTabColours st
        C.TabLog -> WTlg.drawTabLogs st
---------------------------------------------------------------------------------------------------
