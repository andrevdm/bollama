{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Draw
  ( drawUI
  , attrMap
  )
  where

import           Verset

import Brick ((<=>), (<+>))
import Brick.AttrMap qualified as BA
import Brick qualified as B
import Brick.Focus qualified as BF
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Border.Style qualified as BBS
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
  [contentBlock' <=> footer st]

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
  -- <=>
  --(B.padBottom (B.Pad 0) . B.vLimit 1 . B.withAttr (B.attrName "tabSelected") $ B.fill ' ')
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
        C.TabPs     -> drawPsInner st
        C.TabChat   -> drawChatInner st
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
  else BL.renderList (\_ p -> renderPsListItem p) False st._stPs

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
  else BL.renderList (\_ e -> renderModelListItem e) False (st._stModelsList)

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
  drawChatMain
  <=>
  drawChatBottom

  where
    drawChatMain =
      drawChatMainLeft <+> (B.padLeft (B.Pad 4) $ drawChatMainRight)

    drawChatBottom =
      --B.vLimit 1 . B.withAttr (B.attrName "tabFooter") $ B.txt "" <+> B.fill ' '
      B.txt "chatBottom"

    drawChatMainLeft =
      B.txt "chatLeft"

    drawChatMainRight =
      let
        strmTxt =
          case st._stChatCurrent of
            Nothing -> ""
            Just (_chatId, _chat, []) -> ""
            Just (_chatId, _chat, (m: _msgs)) -> m.msgText
      in
      (B.txtWrap strmTxt)
      <=>
      B.fill ' '
      <=>
      --TODO BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent st._stFocusModels == Just C.NChatInputEdit) st._stChatInput
      BE.renderEditor (B.txt . Txt.unlines) (True) st._stChatInput

---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Shared
---------------------------------------------------------------------------------------------------
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
spinnerText st =
  fromMaybe "" $ spinnerFrames `atMay` ((st ^. C.stTick + 0) `mod` length spinnerFrames)

spinner :: C.UiState -> B.Widget n
spinner st =
  B.withAttr (B.attrName "colHeader") . B.txt $ spinnerText st
---------------------------------------------------------------------------------------------------




tabName :: C.Tab -> Text
tabName C.TabModels = "F2: Models"
tabName C.TabPs = "F3: Running"
tabName C.TabChat = "F4: Chat"



attrMap :: BA.AttrMap
attrMap =
  let
    orange = Vty.rgbColor @Int 0xfa 0xa5 0x00
    grey = Vty.rgbColor @Int 128 128 128
    skyBlue = Vty.rgbColor @Int 0x87 0xce 0xeb
    slateBlue = Vty.rgbColor @Int 0x6a 0x5a 0xcd
    pink = Vty.rgbColor @Int 0xff 0xc0 0xcb
  in

  BA.attrMap Vty.defAttr [
      (BE.editAttr             , Vty.black `B.on` grey)
    , (BE.editFocusedAttr      , Vty.black `B.on` Vty.blue)
    , (BL.listSelectedAttr     , B.fg Vty.yellow)
    , (B.attrName "infoTitle"  , B.fg Vty.cyan)
    , (B.attrName "time"       , B.fg Vty.yellow)
    , (B.attrName "colHeader"  , Vty.withStyle (B.fg Vty.blue) Vty.bold)
    , (B.attrName "titleText"  , B.fg Vty.green)
    , (B.attrName "normalText" , B.fg Vty.white)
    , (B.attrName "normalText2", B.fg orange)

    , (B.attrName "footer"          , Vty.black `B.on` grey)
    , (B.attrName "footerTitle"     , Vty.white `B.on` Vty.black)
    , (B.attrName "footerMessage"   , Vty.black `B.on` grey)
    , (B.attrName "version"         , Vty.yellow `B.on` grey)


    , (B.attrName "msgInfo"         , Vty.black `B.on` Vty.blue)
    , (B.attrName "msgError"        , Vty.yellow `B.on` Vty.blue)

    , (B.attrName "tabSelected"     , Vty.black `B.on` Vty.blue)
    , (B.attrName "tabUnselected"   , Vty.black `B.on` grey)
    , (B.attrName "tabFooter"       , Vty.black `B.on` grey)

    ]


spinnerFrames :: [Text]
spinnerFrames = ["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"]
--spinnerFrames = ["-", "\\", "|", "/"]
--spinnerFrames = ["◜", "◠", "◝", "◞", "◡", "◟"]

