{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.PopupEditChat
  ( drawPopupChatEdit
  , handleEventPopupChatEdit
  , mkPopChatEditForm
  ) where

import Verset
import Brick ((<=>), (<+>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick.Forms ((@@=))
import Brick.Forms qualified as BFm
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Control.Lens (Lens', (%=), (.=), use)
import Data.Text qualified as Txt
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Core qualified as C
import Messages qualified as M
import Logging qualified as L
import Storage.Store qualified as Sr
import Utils qualified as U
import Widgets.Common as Wc


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawPopupChatEdit :: C.UiState -> B.Widget C.Name
drawPopupChatEdit st =
  B.vLimit 30 $
  B.hLimit 191 $
  Wc.borderWithLabel' True (fromMaybe "Chat" st._stPopChatEditTitle) $
  B.withAttr (B.attrName "popup") $
  B.padAll 1 $
  ( BFm.renderForm st._stPopChatEditForm
    <=>
    (B.padTop (B.Pad 2) . BC.hCenter $
      (
        let
          focused = BF.focusGetCurrent st._stPopChatEditFocus
          fieldsValid = BFm.allFieldsValid st._stPopChatEditForm

          (attrOk, attrBorderOk) =
            case (fieldsValid, focused) of
              (True, Just C.NDialogOk) -> (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
              (True, _) -> (B.attrName "popupButtonOk", BBS.unicode)

              (False, Just C.NDialogOk) -> (B.attrName "popupButtonDisabledFocused", BBS.unicodeBold)
              (False, _) -> (B.attrName "popupButtonDisabled", BBS.unicode)

          (attrCancel, attrBorderCancel) =
            if focused == Just C.NDialogCancel
            then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
            else (B.attrName "popupButtonCancel", BBS.unicode)
        in
        (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Ok"))
          <+> B.txt " "
          <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Cancel"))
        )
      )
    )
  )


renderPopChatEditModel :: Maybe M.ModelItem -> Bool -> M.ModelItem -> B.Widget C.Name
renderPopChatEditModel origSelected' selected item =
  let
    attrName =
      if selected
      then "listSelectedAttr"
      else "listAttr"
    cs =
      case (.capabilities) <$> item.miShow of
        Just (Just cs') -> Txt.intercalate ", " cs'
        _ -> ""
    usr = item.miTag
    origSelected = (M.miName <$> origSelected') == Just item.miName
  in
  B.vLimit 1 . B.withAttr (B.attrName attrName) $
  B.hBox
    [ col 1 (if origSelected then ">" else " ") ""
    , col 70 item.miName ""
    , colTe col 10 True (maybe "?" (\o -> (U.formatParamSize . U.parseParams $ o.details) <> "  ") item.miShow) ""
    , colTe col 11 True ((maybe "" (\s -> maybe "" show s.modelInfo.llamaContextLength) item.miShow) <> "  ") ""
    , col 40 cs ""
    , col 50 usr ""
    ]


mkPopChatEditForm :: M.ChatEditInfo -> BFm.Form M.ChatEditInfo C.UiEvent C.Name
mkPopChatEditForm cei =
  BFm.newForm
    [ ((col 15 "Name:" "popupHeader") <+>) @@= editTextFieldWithValidate M.ceiName C.NPopChatEditFormName (\n -> (not . Txt.null $ n) && isNameValid n)
    , ((col 15 "Context:" "popupHeader") <+>) @@= editMaybeFieldWithValidate (M.ceiParams . M.cpContextSize) C.NPopChatEditFormCtx (readMaybe @Int . Txt.unpack) show
    , ((col 15 "Temperature:" "popupHeader") <+>) @@= editMaybeFieldWithValidate (M.ceiParams . M.cpTemp) C.NPopChatEditFormTemp (readMaybe @Double . Txt.unpack) show
    , ((col 15 "Model:" "popupHeader") <+>) @@= BFm.listField (\s -> V.fromList s._ceiModels) M.ceiSelectedModel (renderPopChatEditModel cei._ceiSelectedModel) 1 C.NPopChatEditFormModels
    ]
    cei
  where
    isNameValid newName' =
      let oldName = Txt.strip cei._ceiName
          newName = Txt.strip newName'
      in
      if | Txt.null oldName -> True
         | Txt.isInfixOf "#" oldName && Txt.isInfixOf "#" newName -> True
         | not (Txt.isInfixOf "#" oldName) && not (Txt.isInfixOf "#" newName) -> True
         | otherwise -> False


editFieldWithValidate :: forall n a s e. (Ord n, Show n) => Lens' s a -> n -> (Text -> Maybe a) -> (a -> Text) -> s -> BFm.FormFieldState s e n
editFieldWithValidate stLens n validate display =
  let
    limit = Just 1
    renderText = B.txt . Txt.strip . Txt.unlines
    validate' ls' = validate $ Txt.strip . Txt.unlines $ ls'
  in
  BFm.editField stLens n limit display validate' renderText identity



editTextFieldWithValidate :: (Ord n, Show n) => Lens' s Text -> n -> (Text -> Bool) -> s -> BFm.FormFieldState s e n
editTextFieldWithValidate stLens n isValid =
    let
      validate' ls =
        let v = Txt.strip ls in
        if isValid v
          then Just v
          else Nothing
    in editFieldWithValidate stLens n validate' identity



editMaybeFieldWithValidate :: forall n a s e. (Ord n, Show n) => Lens' s (Maybe a) -> n -> (Text -> Maybe a) -> (a -> Text) -> s -> BFm.FormFieldState s e n
editMaybeFieldWithValidate stLens n validate display =
  let
    validate' t =
      if Txt.null t
        then Just Nothing
        else
          case validate t of
            Nothing -> Nothing
            Just v' -> Just (Just v')

    display' :: Maybe a -> Text
    display' a = maybe "" display a
  in
  editFieldWithValidate stLens n validate' display'
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleEventPopupChatEdit :: BCh.BChan C.Command -> B.BrickEvent C.Name C.UiEvent -> Vty.Event -> B.EventM C.Name C.UiState ()
handleEventPopupChatEdit _commandChan ev ve = do
  st <- B.get
  let focused = BF.focusGetCurrent st._stPopChatEditFocus

  case ve of
    Vty.EvKey k ms -> do
      case (focused, k, ms) of
        (_, Vty.KChar 'q', [Vty.MCtrl]) -> do
          clear

        (_, Vty.KEsc, []) -> do
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

        (_, Vty.KChar '\t', []) -> do
          C.stPopChatEditFocus %= BF.focusNext
          updateFocus

        (_, Vty.KBackTab, []) -> do
          C.stPopChatEditFocus %= BF.focusPrev
          updateFocus

        (Just C.NDialogOk, Vty.KEnter, []) -> do
          if BFm.allFieldsValid st._stPopChatEditForm
            then do
              let f = BFm.formState st._stPopChatEditForm
              case f._ceiSelectedModel of
                Just model ->
                  st._stPopChatEditOnOk f._ceiName model f._ceiParams
                Nothing -> do
                  store <- use C.stStore
                  liftIO . store.swLog.lgError $ "No model selected"
              clear
            else do
              pass

        (Just C.NDialogCancel, Vty.KEnter, []) -> do
          C.stPopup .= Nothing
          C.stPopChatEditTitle .= Nothing

        _ -> B.zoom C.stPopChatEditForm $ BFm.handleFormEvent ev

      pass
    _ -> pass

  where
    clear = do
      C.stPopup .= Nothing
      C.stPopChatEditTitle .= Nothing
      C.stPopChatEditFocus %= BF.focusSetCurrent C.NPopChatEditFormName

    updateFocus :: B.EventM C.Name C.UiState ()
    updateFocus = do
      st <- B.get
      let focused = BF.focusGetCurrent st._stPopChatEditFocus

      case focused of
        Nothing -> pass
        Just C.NDialogOk -> pass
        Just C.NDialogCancel -> pass
        Just n -> C.stPopChatEditForm %= BFm.setFormFocus n
----------------------------------------------------------------------------------------------------------------------



