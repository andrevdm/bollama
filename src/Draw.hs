{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Draw
  ( drawUI
  , mkPopChatEditForm
  , vBoxWithPadding
  )
  where

import           Verset

import Brick ((<=>), (<+>))
import Brick.AttrMap qualified as BA
import Brick qualified as B
import Brick.Forms ((@@=))
import Brick.Forms qualified as BFm
import Brick.Focus qualified as BF
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.Core qualified as BW
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Lens (Lens', (^.))
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Ollama qualified as O

import Core qualified as C
import Config qualified as Cfg
import Utils qualified as U



---------------------------------------------------------------------------------------------------
-- Main draw function
---------------------------------------------------------------------------------------------------
drawUI :: C.UiState -> [B.Widget C.Name]
drawUI st =
  [ case st._stErrorMessage of
      -- Error message wins
      Just _ -> BC.centerLayer $ drawErrorMessage st
      -- Otherwise draw current popup if there is one
      Nothing ->
        BC.centerLayer $
          case st._stPopup of
            Nothing -> BW.emptyWidget
            Just C.PopupChatEdit -> drawPopupChatEdit st
            Just C.PopupPrompt -> drawPopupPrompt st
            Just C.PopupConfirm -> drawPopupConfirm st

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
              name = tabName t
              isSelected = st._stTab == t
              attr = if isSelected then "tabSelected" else "tabUnselected"
            in
            B.withAttr (B.attrName attr) $ B.txt name
        )

    drawTabsContent =
      case st._stTab of
        C.TabModels -> drawModelsInner st
        C.TabPs -> drawPsInner st
        C.TabChat -> drawChatInner st
        C.TabColours -> drawColours st
        C.TabLog -> drawLogs st
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Ps
---------------------------------------------------------------------------------------------------
drawPsInner :: C.UiState -> B.Widget C.Name
drawPsInner st =
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
  then (spinner st <=> B.fill ' ')
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
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Models
---------------------------------------------------------------------------------------------------
drawModelsInner :: C.UiState -> B.Widget C.Name
drawModelsInner st =
  (
    B.vLimit 1 $ B.hBox [
        col 70 "Name" "colHeader"
      , col 11 "Params" "colHeader"
      , col 11 "Quant" "colHeader"
      , col 11 "Context" "colHeader"
      , col 11 "Size" "colHeader"
      , col 17 "Family" "colHeader"
      , col 40 "Capabilities" "colHeader"
      , col 50 "User" "colHeader"
      ]
  )
  <=>
  if st._stModelListLoading
  then spinner st
  else
    BL.renderList (\_ e -> renderModelListItem e) (BF.focusGetCurrent st._stFocusModels == Just C.NModelsList) (st._stModelsList)

  where
    renderModelListItem :: C.ModelItem -> B.Widget C.Name
    renderModelListItem itm =
      let
        mi = itm.miInfo
        sm = itm.miShow
        cs =
          case (.capabilities) <$> sm of
            Just (Just cs') -> Txt.intercalate ", " cs'
            _ -> ""
        usr = fromMaybe "" $ Map.lookup itm.miName st._stAppConfig.acModelTag
      in
      B.vLimit 1 $ B.hBox [
          colTb col  70 mi.name ""
        , colTe col  11 (maybe (spinnerText st) (.details.parameterSize) sm) ""
        , colTe col  11 (maybe "" (.details.quantizationLevel) sm) ""
        , colTe col  11 (((.modelInfo.llamaContextLength) <$> sm) & join & maybe "" show) ""
        , colTb col  11 (U.bytesToGb mi.size) ""
        , colTe col  17 (maybe "" (.details.familiy) sm) ""
        , colTe col  40 cs ""
        , colTe col  50 usr ""
        ]
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Chat
---------------------------------------------------------------------------------------------------
drawChatInner :: C.UiState -> B.Widget C.Name
drawChatInner st =
  drawChatTop
  <=>
  drawChatMain
  <=>
  drawChatBottom

  where
    drawChatMain =
      drawChatMainLeft <+> (B.padLeft (B.Pad 4) $ drawChatMainRight)

    drawChatTop =
      B.txt "chatTop"

    drawChatBottom =
      B.txt "chatBottom"

    drawChatMainLeft =
      let selected = BF.focusGetCurrent st._stFocusChat == Just C.NChatsList
      in
      B.hLimit 50 $
      borderWithLabel' selected "Chats" $
      BL.renderList renderChatItem selected (st._stChatsList)

    drawChatMainRight =
      let
        inputEditSelected = BF.focusGetCurrent st._stFocusChat == Just C.NChatInputEdit
        modelName = fromMaybe "" $ st._stChatCurrent <&> (C.chatModel) . fst
      in
      borderWithLabel' False "Conversation"
      ( (B.withAttr (B.attrName "colHeader") $ B.txt "Model: ") <+> (B.txt modelName)
        <=>
        B.txt " "
        <=>
        ( let ws = zip [0..] st._stChatMsgs <&> \(ix, msg) -> renderChatMsgItem False ix False msg
          in
          B.withClickableVScrollBars C.VScrollClick . B.withVScrollBarHandles . B.withVScrollBars B.OnRight $
          B.viewport C.NChatScroll B.Vertical . B.vBox $ ws
        )
      )
      <=>
      B.vLimit 8
      (
        borderWithLabel' inputEditSelected "Input (ctrl-s to send)"
        ( case (snd <$> st._stChatCurrent) of
            Nothing -> B.fill ' '
            Just C.SsNotStreaming -> (BE.renderEditor (B.txt . Txt.unlines) inputEditSelected st._stChatInput)
            Just C.SsStreaming -> spinner2 st <+> B.fill ' '
        )
      )


    renderChatMsgItem :: Bool -> Int -> Bool -> C.ChatMessage -> B.Widget C.Name
    renderChatMsgItem listSelected ix itemSelected msg =
      let attrName =
            if listSelected && itemSelected
            then "chatMsgSelected"
            else if ix `mod` 2 == 0 then "chatMsgA" else "chatMsgB"
          msgTxt = U.removeThink msg.msgText
      in
      B.padBottom (B.Pad 1) $
      B.withAttr (B.attrName attrName) $
      B.hBox
        [ col 15 (show msg.msgRole) attrName
        , B.vLimit 50 $ B.txtWrap msgTxt
        ]


    renderChatItem :: Bool -> C.Chat -> B.Widget C.Name
    renderChatItem _selected chat =
      let defaultMarker =
            if Just chat.chatName == st._stAppConfig.acDefaultChat || Just (C.unChatId chat.chatId) == st._stAppConfig.acDefaultChat
            then B.withAttr (B.attrName "chatDefaultMarker") $ B.txt "*"
            else B.txt " "
      in
      B.vLimit 1 $
      defaultMarker <+> B.txt chat.chatName
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Colours
---------------------------------------------------------------------------------------------------
drawColours :: C.UiState -> B.Widget C.Name
drawColours st =
  BL.renderList go True st._stColoursList

  where
    go :: Bool -> Text -> B.Widget C.Name
    go _ n =
      B.hBox
        [ col 20 n ""
        , col 30 "abcdefgABCDEFG123456()_" (Txt.unpack $ "_fg_" <> n)
        , col 30 "                       " (Txt.unpack $ "_bg_" <> n)
        ]
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Log
---------------------------------------------------------------------------------------------------
drawLogs :: C.UiState -> B.Widget C.Name
drawLogs st =
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
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Shared
---------------------------------------------------------------------------------------------------
borderWithLabel' :: Bool -> Text -> B.Widget n -> B.Widget n
borderWithLabel' selected txt' w =
  let styler = if selected then (B.withAttr (B.attrName "borderSelectedLabel")) else identity
  in
  BB.borderWithLabel (styler $ B.txt txt') $
  w


colTe :: (Int -> Text -> t -> t1) -> Int -> Text -> t -> t1
colTe colF width txt' attr =
  if (Txt.length txt' > width)
    then colF width (Txt.take (width - 3) txt' <> "...") attr
    else colF width txt' attr

colTb :: (Int -> Text -> t -> t1) -> Int -> Text -> t -> t1
colTb colF width txt' attr =
  if (Txt.length txt' > width)
    then colF width ("..." <> Txt.take (width - 3) txt') attr
    else colF width txt' attr

col :: Int -> Text -> [Char] -> B.Widget n
col width txt' attr =
  (B.vLimit 1 . B.hLimit width $ (B.withAttr (BA.attrName attr) $ B.txt txt') <+> B.fill ' ')

--col' :: Int -> Text -> [Char] -> B.Widget n
--col' width txt' attr =
--  (B.vLimit 1 . B.hLimit width $ B.fill ' ' <+> (B.withAttr (BA.attrName attr) $ B.txt txt'))

spinnerText :: C.UiState -> Text
spinnerText st = fromMaybe "" $ spinnerFrames `atMay` ((st ^. C.stTick + 0) `mod` length spinnerFrames)

spinner :: C.UiState -> B.Widget n
spinner st = B.withAttr (B.attrName "spinner1") . B.txt $ spinnerText st


spinnerText2 :: C.UiState -> Text
spinnerText2 st = fromMaybe "" $ spinnerFrames2 `atMay` ((st ^. C.stTick + 0) `mod` length spinnerFrames2)

spinner2 :: C.UiState -> B.Widget n
spinner2 st = B.withAttr (B.attrName "spinner2") . B.txt $ spinnerText2 st
---------------------------------------------------------------------------------------------------



tabName :: C.Tab -> Text
tabName C.TabModels = "F2: Models"
tabName C.TabPs = "F3: Running"
tabName C.TabChat = "F4: Chat"
tabName C.TabColours = "F11: Colours"
tabName C.TabLog = "F12: Log"




---------------------------------------------------------------------------------------------------
-- Popup Chat Edit
---------------------------------------------------------------------------------------------------
drawPopupChatEdit :: C.UiState -> B.Widget C.Name
drawPopupChatEdit st =
  B.vLimit 30 $
  B.hLimit 180 $
  borderWithLabel' True (fromMaybe "Chat" st._stPopChatEditTitle) $
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


renderPopChatEditModel :: Maybe C.ModelItem -> Bool -> C.ModelItem -> B.Widget C.Name
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
    origSelected = (C.miName <$> origSelected') == Just item.miName
  in
  B.vLimit 1 . B.withAttr (B.attrName attrName) $
  B.hBox
    [ col 1 (if origSelected then ">" else " ") ""
    , col 70 item.miName ""
    , col 11 (maybe "?" (.details.parameterSize) item.miShow) ""
    , col 40 cs ""
    , col 50 usr ""
    ]


mkPopChatEditForm :: C.ChatEditInfo -> BFm.Form C.ChatEditInfo C.UiEvent C.Name
mkPopChatEditForm cei =
  BFm.newForm
    [ ((col 15 "Name:" "popupHeader") <+>) @@= editTextFieldWithValidate C.ceiName C.NPopChatEditFormName (not . Txt.null)
    , ((col 15 "Context:" "popupHeader") <+>) @@= BFm.editShowableField C.ceiContext C.NPopChatEditFormCtx
    , ((col 15 "Temperature:" "popupHeader") <+>) @@= BFm.editShowableField C.ceiTemp C.NPopChatEditFormTemp
    , ((col 15 "Model:" "popupHeader") <+>) @@= BFm.listField (\s -> V.fromList s._ceiModels) C.ceiSelectedModel (renderPopChatEditModel cei._ceiSelectedModel) 1 C.NPopChatEditFormModels
    ]
    cei

editTextFieldWithValidate :: (Ord n, Show n) => Lens' s Text -> n -> (Text -> Bool) -> s -> BFm.FormFieldState s e n
editTextFieldWithValidate stLens n isValid =
    let validate ls =
            let v = Txt.strip . Txt.unlines $ ls in
            if isValid v
            then pure v
            else Nothing
        limit = Just 1
        renderText = B.txt . Txt.unlines
    in BFm.editField stLens n limit identity validate renderText identity

---------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Popup Prompt
---------------------------------------------------------------------------------------------------
drawPopupPrompt :: C.UiState -> B.Widget C.Name
drawPopupPrompt st =
  B.vLimit 10 $
  B.hLimit 180 $
  borderWithLabel' True (fromMaybe "Enter text" st._stPopPromptTitle) $
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
---------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Popup Confirm
---------------------------------------------------------------------------------------------------
drawPopupConfirm :: C.UiState -> B.Widget C.Name
drawPopupConfirm st =
  B.vLimit 10 $
  B.hLimit 180 $
  borderWithLabel' True (fromMaybe "Confirm" st._stPopConfirmTitle) $
  B.withAttr (B.attrName "popup") $
  B.padAll 1 $
  ( ( B.withAttr (B.attrName "popupHeader") $
      B.txt (fromMaybe "Are you sure?" st._stPopConfirmTitle)
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
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Ok"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Cancel"))
      )
    )
  )
---------------------------------------------------------------------------------------------------





---------------------------------------------------------------------------------------------------
-- Error message
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


vBoxWithPadding :: Int -> [B.Widget n] -> B.Widget n
vBoxWithPadding n xs = B.vBox $ xs <&> \x -> B.padBottom (B.Pad n) x




spinnerFrames :: [Text]
spinnerFrames = ["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"]
--spinnerFrames = ["-", "\\", "|", "/"]
--spinnerFrames = ["◜", "◠", "◝", "◞", "◡", "◟"]

spinnerFrames2 :: [Text]
--spinnerFrames2 = ["█▒▒▒▒▒▒▒▒▒", "███▒▒▒▒▒▒▒", "█████▒▒▒▒▒", "███████▒▒▒", "██████████"]
spinnerFrames2 =
  let x =
       [ "🖋️"
       , ".🖋️"
       , "..🖋️"
       , "...🖋️"
       , "....🖋️"
       , ".....🖋️"
       , "......🖋️"
       , ".......🖋️"
       , "........🖋️"
       , ".........🖋️"
       , "..........🖋️"
       , "...........🖋️"
       , "............🖋️"
       , ".............🖋️"
       , "..............🖋️"
       , "...............🖋️"
       , "................🖋️"
       , ".................🖋️"
       , "..................🖋️"
       , "...................🖋️"
       , "....................🖋️"
       , ".....................🖋️"
       , "......................🖋️"
       , ".......................🖋️"
       , "........................🖋️"
       ]
  in
  x
