{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.Common where

import Verset
import Brick ((<+>))
import Brick.AttrMap qualified as BA
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Control.Lens ((^.))
import Data.Text qualified as Txt

import Core qualified as C



tabName :: C.Tab -> Text
tabName C.TabModels = "F2: Models"
tabName C.TabPs = "F3: Running"
tabName C.TabChat = "F4: Chat"
tabName C.TabColours = "F11: Colours"
tabName C.TabLog = "F12: Log"


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
spinnerText st =
  let frames = spinnerFrames st._stAppConfig.acAvoidEmojis in
  fromMaybe "" $ frames `atMay` ((st ^. C.stTick + 0) `mod` length frames)


spinner :: C.UiState -> B.Widget n
spinner st = B.withAttr (B.attrName "spinner1") . B.txt $ spinnerText st


spinnerText2 :: C.UiState -> Text
spinnerText2 st =
  let frames = spinnerFrames2 st._stAppConfig.acAvoidEmojis in
  fromMaybe "" $ frames `atMay` ((st ^. C.stTick + 0) `mod` length frames)

spinner2 :: C.UiState -> B.Widget n
spinner2 st = B.withAttr (B.attrName "spinner2") . B.txt $ spinnerText2 st


vBoxWithPadding :: Int -> [B.Widget n] -> B.Widget n
vBoxWithPadding n xs = B.vBox $ xs <&> \x -> B.padBottom (B.Pad n) x


spinnerFrames :: Bool -> [Text]
spinnerFrames False = ["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"]
spinnerFrames True = ["-", "\\", "|", "/"]

spinnerFrames2 :: Bool -> [Text]
spinnerFrames2 avoidEmojis =
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
  if avoidEmojis
  then Txt.replace "🖋️" "w" <$> x
  else x
