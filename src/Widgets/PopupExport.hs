{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupExport
  ( drawPopupExport
  , handleEventPopupExport
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
import Brick.Widgets.FileBrowser qualified as BFi
import Control.Lens (Lens', (^.), (.=), (%=), use, to)
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Graphics.Vty qualified as Vty
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as Fp

import Core qualified as C
import Messages qualified as M
import Widgets.Common as Wc


---------------------------------------------------------------------------------------------------
-- Draw
---------------------------------------------------------------------------------------------------
drawPopupExport :: C.UiState -> B.Widget C.Name
drawPopupExport st =
  B.vLimit 50 $
  B.hLimit 120 $
  Wc.borderWithLabel' True "Export Chat" $
  B.padAll 1 $
  ( ( BB.border (BFi.renderFileBrowser (BF.focusGetCurrent st._stPopExportFocus == Just C.NPopExportBrowser) st._stPopExportBrowser)
      <=>
      (B.padTop (B.Pad 1) . B.vLimit 1 $ (B.withAttr (B.attrName "colHeader") (B.txt "dir : ") <+> BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent st._stPopExportFocus == Just C.NPopExportDir) st._stPopExportDir))
      <=>
      (B.padTop (B.Pad 1) . B.vLimit 1 $ B.vLimit 1 (B.withAttr (B.attrName "colHeader") (B.txt "name: ") <+> BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent st._stPopExportFocus == Just C.NPopExportFileName) st._stPopExportFName))
      <=>
      (radio C.NPopExportFormatJson "JSON" 8 M.ExportJson C.stPopExportFormat)
      <=>
      (radio C.NPopExportFormatText "Text" 8 M.ExportText C.stPopExportFormat)
      <=>
      (case st._stPopExportError of
        Nothing -> B.emptyWidget
        Just err -> B.padTop (B.Pad 1) . B.withAttr (B.attrName "popupError") $ B.txt err
      )
    )
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          if BF.focusGetCurrent st._stPopExportFocus == Just C.NDialogOk
          then (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonOk", BBS.unicode)

        (attrCancel, attrBorderCancel) =
          if BF.focusGetCurrent st._stPopExportFocus == Just C.NDialogCancel
          then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonCancel", BBS.unicode)
      in
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Yes"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "No"))
      )
    )
  )

  where
    radio :: (Eq a) => C.Name -> Text -> Int -> a -> Lens' C.UiState a -> B.Widget C.Name
    radio name label labelWidth value lens =
      let
        focused = BF.focusGetCurrent st._stPopExportFocus == Just name
        attr = if focused then "radioFocused" else "radio"
        checked = st ^.lens == value
      in
      B.padTop (B.Pad 1) . B.vLimit 1 $
      ( col labelWidth (label <> ": ") "colHeader" <+> B.withAttr (B.attrName attr) (B.txt (if checked then "[X]" else "[ ]"))
      )
---------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupExport :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupExport _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopExportFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          C.stPopup .= Nothing

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopExportFocus %= BF.focusNext
          updateBrowserFromInputs

        (_, Vty.KBackTab, []) -> do
          C.stPopExportFocus %= BF.focusPrev
          updateBrowserFromInputs

        -- Suppress the space key / select file
        (Just C.NPopExportBrowser, Vty.KChar ' ', _) -> do
          pass
        --
        -- Suppress the search key for now
        (Just C.NPopExportBrowser, Vty.KChar '/', _) -> do
          pass

        -- Enter = go to dir
        (Just C.NPopExportBrowser, Vty.KEnter, _) -> do
          goToDir

        (Just C.NPopExportFormatJson, Vty.KEnter, []) -> do
          C.stPopExportFormat .= M.ExportJson

        (Just C.NPopExportFormatJson, Vty.KChar ' ', []) -> do
          C.stPopExportFormat .= M.ExportJson

        (Just C.NPopExportFormatText, Vty.KEnter, []) -> do
          C.stPopExportFormat .= M.ExportText

        (Just C.NPopExportFormatText, Vty.KChar ' ', []) -> do
          C.stPopExportFormat .= M.ExportText

        (Just C.NPopExportBrowser, _, _) -> do
          C.stPopExportError .= Nothing
          B.zoom C.stPopExportBrowser $ BFi.handleFileBrowserEvent ve
          updateFromBrowser

        (Just C.NPopExportFileName, _, _) -> do
          B.zoom C.stPopExportFName $ BE.handleEditorEvent ev

        (Just C.NPopExportDir, _, _) -> do
          B.zoom C.stPopExportDir $ BE.handleEditorEvent ev

        (Just C.NDialogOk, Vty.KEnter, []) ->
          ok

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          clear

        _ -> pass

    _ -> pass

  where
    ok :: B.EventM C.Name C.UiState ()
    ok = do
      dir <- use (C.stPopExportDir . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)
      fname <- use (C.stPopExportFName . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

      case (Txt.null dir, Txt.null fname) of
        (False, False) -> do
          st <- B.get
          clear
          st._stPopExportOnOk st._stPopExportFormat $ Txt.unpack dir </> Txt.unpack fname

        _ -> do
          C.stPopExportError .= Just "Empty dir or file name"


    goToDir = do
      browser <- use C.stPopExportBrowser
      case BFi.fileBrowserCursor browser of
        Nothing -> pass
        Just h -> do
          liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
            False -> pass
            True -> do
              browser2 <- liftIO $ BFi.setWorkingDirectory h.fileInfoFilePath browser
              C.stPopExportBrowser .= browser2


    clear = do
      st <- B.get
      browser2 <- liftIO $ BFi.newFileBrowser (const True) C.NPopExportBrowser (st._stAppConfig.acDefaultExportDir)
      let dir = BFi.getWorkingDirectory browser2
      C.stPopup .= Nothing
      C.stPopExportFocus %= BF.focusSetCurrent C.NPopExportBrowser
      C.stPopExportOnOk .= (const . const $ pass)
      C.stPopExportBrowser .= browser2
      C.stPopExportError .= Nothing
      C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing
      C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack dir] Nothing


    updateFromBrowser = do
      browser <- use C.stPopExportBrowser
      case BFi.fileBrowserCursor browser of
        Nothing -> do
          C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [] Nothing
          C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing
        Just h -> do
          liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
            True -> do
              C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack h.fileInfoFilePath] Nothing
              C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [] Nothing

            False -> do
              C.stPopExportDir . BE.editContentsL .= TxtZ.textZipper [Txt.pack . Fp.takeDirectory $ h.fileInfoFilePath] Nothing
              C.stPopExportFName . BE.editContentsL .= TxtZ.textZipper [Txt.pack $ h.fileInfoSanitizedFilename] Nothing


    updateBrowserFromInputs = do
      st <- B.get
      case BF.focusGetCurrent st._stPopExportFocus of
        Just C.NPopExportBrowser -> do
          browser1 <- use C.stPopExportBrowser

          prevDir <-
            case BFi.fileBrowserCursor browser1 of
              Nothing -> pure Nothing
              Just h -> do
                liftIO (Dir.doesDirectoryExist h.fileInfoFilePath) >>= \case
                  True -> pure . Just . Txt.pack $ h.fileInfoFilePath
                  False -> pure . Just . Txt.pack $ Fp.takeDirectory h.fileInfoFilePath

          dir <- use (C.stPopExportDir . BE.editContentsL . to TxtZ.getText . to Txt.unlines . to Txt.strip)

          when (not (Txt.null dir) || Just dir /= prevDir) $ do
            browser2 <- liftIO $ BFi.setWorkingDirectory (Txt.unpack dir) browser1
            C.stPopExportBrowser .= browser2

        _ -> do
          pass
----------------------------------------------------------------------------------------------------------------------



