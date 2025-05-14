{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.TabColours
  ( drawTabColours
  , handleTabColours
  ) where

import Verset
import Brick.BChan qualified as BCh
import Brick qualified as B
import Brick.Widgets.List qualified as BL
import Data.Text qualified as Txt
import Graphics.Vty qualified as Vty

import Core qualified as C
import Widgets.Common as Wc


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawTabColours :: C.UiState -> B.Widget C.Name
drawTabColours st =
  BL.renderList go True st._stColoursList

  where
    go :: Bool -> Text -> B.Widget C.Name
    go _ n =
      B.hBox
        [ col 20 n ""
        , col 30 "abcdefgABCDEFG123456()_" (Txt.unpack $ "_fg_" <> n)
        , col 30 "                       " (Txt.unpack $ "_bg_" <> n)
        ]
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleTabColours
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabColours _commandChan _ev ve _focused _k _ms = do
  B.zoom C.stColoursList $ BL.handleListEventVi BL.handleListEvent ve
----------------------------------------------------------------------------------------------------------------------



