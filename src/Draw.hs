{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Draw
  ( drawUI
  --, attrMap
  )
  where

import           Verset

import Brick ((<=>), (<+>))
import Brick.AttrMap qualified as BA
import Brick qualified as B
import Brick.Focus qualified as BF
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.Core qualified as BW
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Lens ((^.))
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Data.Map.Strict qualified as Map
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Core qualified as C
import Config qualified as Cfg
import Utils qualified as U



---------------------------------------------------------------------------------------------------
-- Main draw function
---------------------------------------------------------------------------------------------------
drawUI :: C.UiState -> [B.Widget C.Name]
drawUI st =
  [ BC.centerLayer $
      case st._stPopup of
        Nothing -> BW.emptyWidget
        Just C.PopupChatEdit -> drawPopupChatEdit st

  , contentBlock' <=> footer st
  ]

  where
    contentBlock' = drawTabs st
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Footer
---------------------------------------------------------------------------------------------------
footer :: C.UiState -> B.Widget C.Name
footer st =
  B.vLimit 1 . B.withAttr (B.attrName "footer") $ B.txt (Cfg.verText <> " | ") <+> drawFooterInput <+> B.fill ' '

  where
    drawFooterInput =
      case st._stFooterWidget of
        Nothing -> B.txt st._stDebug
        Just (_name', fwidget) -> fwidget st
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
      B.hLimit 40 $
      borderWithLabel' selected "Chats" $
      BL.renderList renderChatItem selected (st._stChatsList)

    drawChatMainRight =
      let
        inputEditSelected = BF.focusGetCurrent st._stFocusChat == Just C.NChatInputEdit
        msgListSelected = BF.focusGetCurrent st._stFocusChat == Just C.NChatMsgList
      in
      borderWithLabel' msgListSelected "Conversation"
      ( BL.renderListWithIndex (renderChatListItem msgListSelected) msgListSelected (st._stChatMsgList)
      )
      <=>
      B.vLimit 8
      (
        borderWithLabel' inputEditSelected "Input"
        ( case (snd <$> st._stChatCurrent) of
            Nothing -> B.fill ' '
            Just C.SsNotStreaming -> (BE.renderEditor (B.txt . Txt.unlines) inputEditSelected st._stChatInput)
            Just C.SsStreaming -> spinner2 st <+> B.fill ' '
        )
      )


    renderChatListItem :: Bool -> Int -> Bool -> C.ChatMessage -> B.Widget C.Name
    renderChatListItem listSelected ix itemSelected msg =
      let attrName =
           if listSelected && itemSelected
           then "chatMsgSelected"
           else
             if ix `mod` 2 == 0 then "chatMsgA" else "chatMsgB" in

      B.padBottom (B.Pad 1) $
      B.withAttr (B.attrName attrName) $
      B.hBox
        [ col 15 (show msg.msgRole) attrName
        , B.txtWrap msg.msgText
        ]


    renderChatItem :: Bool -> C.Chat -> B.Widget C.Name
    renderChatItem _selected chat =
      B.vLimit 1 $
      B.txt chat.chatName

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

col' :: Int -> Text -> [Char] -> B.Widget n
col' width txt' attr =
  (B.vLimit 1 . B.hLimit width $ B.fill ' ' <+> (B.withAttr (BA.attrName attr) $ B.txt txt'))

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
  B.vLimit 25 $
  B.hLimit 180 $
  borderWithLabel' True "Chat" $
  B.withAttr (B.attrName "popup") $
  B.padAll 1 $
  ( B.vBox
    [ B.vLimit 1 $ B.hBox
        [ col 7 "Name:" "popupHeader"
        , BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent st._stPopChatEditFocus == Just C.NPopChatEditName) st._stPopChatEditName
        ]
    , B.txt " "
    , B.vLimit 1 $ B.hBox
        [ col 7 "Model:" "popupHeader"
        , B.hBox [col 70 "Name" "popupTableHeader", col 11 "Params" "popupTableHeader", col 40 "Capabilities" "popupTableHeader", col 50 "User" "popupTableHeader"]
        ]
    , B.vLimit 12 $
      ( B.padLeft (B.Pad 7) $
        B.withAttr (B.attrName "listAttr") $
        BL.renderList renderModel (BF.focusGetCurrent st._stPopChatEditFocus == Just C.NPopChatEditModels) st._stPopChatEditModels
      )
    ]
    <=> B.fill ' '
    <=>
    ( B.padTop (B.Pad 2) . BC.hCenter $
      let
        (attrOk, attrBorderOk) =
          if BF.focusGetCurrent st._stPopChatEditFocus == Just C.NPopChatEditOk
          then (B.attrName "popupButtonOkFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonOk", BBS.unicode)

        (attrCancel, attrBorderCancel) =
          if BF.focusGetCurrent st._stPopChatEditFocus == Just C.NPopChatEditCancel
          then (B.attrName "popupButtonCancelFocused", BBS.unicodeBold)
          else (B.attrName "popupButtonCancel", BBS.unicode)
      in
      (     B.withBorderStyle attrBorderOk (BB.border (B.withAttr attrOk . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Ok"))
        <+> B.txt " "
        <+> B.withBorderStyle attrBorderCancel (BB.border (B.withAttr attrCancel . B.vLimit 1 . B.hLimit 8 . BC.hCenter $ B.txt "Cancel"))
      )
    )
  )

  where
    renderModel :: Bool -> C.ModelItem -> B.Widget C.Name
    renderModel selected item =
      let
        attrName =
          if selected
          then "listSelectedAttr"
          else "listAttr"
        cs =
          case (.capabilities) <$> item.miShow of
            Just (Just cs') -> Txt.intercalate ", " cs'
            _ -> ""
        usr = fromMaybe "" $ Map.lookup item.miName st._stAppConfig.acModelTag
      in
      B.withAttr (B.attrName attrName) $
      B.hBox
        [ col 70 item.miName ""
        , col 11 (maybe (spinnerText st) (.details.parameterSize) item.miShow) ""
        , col 40 cs ""
        , col 50 usr ""
        ]
---------------------------------------------------------------------------------------------------





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
