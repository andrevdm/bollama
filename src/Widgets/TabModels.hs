{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Widgets.TabModels
  ( drawTabModels
  , handleTabModels
  , handleAppEventGotModelList
  , handleAppEventGotModelShow
  , handleAppEventModelShowDone
  , filterModels
  , refreshModelsList
  , refreshModelsShow
  ) where

import Verset
import Brick ((<=>))
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception.Safe (catch)
import Control.Lens ((%=), (.=), (^?), (^.), at, use, to)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time qualified as DT
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import Ollama qualified as O

import Config qualified as Cfg
import Core qualified as C
import Logging qualified as L
import Messages qualified as M
import Storage.Store qualified as Sr
import Utils qualified as U
import Widgets.Common as Wc
import Widgets.TabChat qualified as WTct


----------------------------------------------------------------------------------------------------------------------
-- Draw
----------------------------------------------------------------------------------------------------------------------
drawTabModels :: C.UiState -> B.Widget C.Name
drawTabModels st =
  (
    B.vLimit 1 $ B.hBox [
        col 70 "Name" "colHeader"
      , col 8 "Params" "colHeader"
        , B.txt "  "
      , col 11 "Quant" "colHeader"
      , col 11 "Context" "colHeader"
        , B.txt "  "
      , col 8 "Size" "colHeader"
        , B.txt "  "
      , col 17 "Family" "colHeader"
      , col 40 "Capabilities" "colHeader"
      , col 50 "User" "colHeader"
      ]
  )
  <=>
  if st._stModelListLoading
  then Wc.spinner st
  else
    BL.renderList (\_ e -> renderModelListItem e) (BF.focusGetCurrent st._stFocusModels == Just C.NModelsList) (st._stModelsList)

  where
    renderModelListItem :: M.ModelItem -> B.Widget C.Name
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
          colTb col 70 mi.name ""
        , colTe col 8 True (maybe (Wc.spinnerText st) (\o -> (U.formatParamSize . U.parseParams $ o.details)) sm) ""
        , B.txt "  "
        , colTe col 11 False (maybe "" (.details.quantizationLevel) sm) ""
        , colTe col 11 True (((.modelInfo.llamaContextLength) <$> sm) & join & maybe "" show) ""
        , B.txt "  "
        , colTe col 8 True (U.bytesToGb mi.size) ""
        , B.txt "  "
        , colTe col 17 False (maybe "" (.details.familiy) sm) ""
        , colTe col 40 False cs ""
        , colTe col 50 False usr ""
        ]
----------------------------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------------------------
handleTabModels
  :: BCh.BChan C.Command
  -> B.BrickEvent C.Name C.UiEvent
  -> Vty.Event
  -> Maybe C.Name
  -> Vty.Key
  -> [Vty.Modifier]
  -> B.EventM C.Name C.UiState ()
handleTabModels commandChan _ev ve focused k ms =
  case (focused, k, ms) of
    (Just C.NModelsList, Vty.KChar '/', []) -> do
      currentFilter <- use C.stModelsFilter
      C.stPopup .= Just C.PopupPrompt
      C.stPopPromptEdit . BE.editContentsL .= TxtZ.textZipper [currentFilter] Nothing
      C.stPopPromptTitle .= Just "Model filter"
      C.stPopPromptOnOk .= \txt -> do
        C.stModelsFilter .= txt
        filteredModels <- filterModels
        wasSelected <- B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName)
        let ix = findIndex (\x -> Just x.miName == wasSelected) filteredModels
        C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix

    (Just C.NModelsList, Vty.KChar 'c', []) -> do
      tags <- do
        cfg <- use C.stAppConfig
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName) >>= \case
          Nothing -> pure ""
          Just selected -> pure . maybe "" (Txt.strip . Txt.replace "\n" " " . Txt.replace "\r" " ") $ Map.lookup selected cfg.acModelTag

      C.stPopup .= Just C.PopupPrompt
      C.stPopPromptEdit . BE.editContentsL .= TxtZ.textZipper [tags] Nothing
      C.stPopPromptTitle .= Just "User comment edit"
      C.stPopPromptOnOk .= \txt -> do
        B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName) >>= \case
          Nothing -> pass
          Just selected -> do
            C.stAppConfig %= \cfg2 -> cfg2 { C.acModelTag = Map.insert selected txt cfg2.acModelTag }
            liftIO . Cfg.writeAppConfig =<< use C.stAppConfig

    (Just C.NModelsList, Vty.KChar '*', []) -> do
      B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName) >>= \case
        Nothing -> pass
        Just selected -> do
          C.stAppConfig %= \cfg2 -> cfg2 { C.acDefaultModel = Just selected }
          liftIO . Cfg.writeAppConfig =<< use C.stAppConfig
          U.setFooterMessage 8 $ "Default model set to: " <> selected

    (Just C.NModelsList, Vty.KChar 'd', []) -> do
      B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName) >>= \case
        Nothing -> pass
        Just selected -> do
          C.stPopup .= Just C.PopupConfirm
          C.stPopConfirmTitle .= Just "Are you sure you want to remove this model?"
          C.stPopConfirmDetail .= Just ("   " <> selected)
          C.stPopConfirmOnOk .= do
            catch
              (do
                 st <- B.get
                 liftIO $ O.deleteModelOps (Just $ fromMaybe C.ollamaDefaultUrl st._stAppConfig.acOllamaUrl) selected
                 liftIO $ BCh.writeBChan commandChan C.CmdRefreshModelList
              )
              (\(e :: SomeException) -> do
                st <- B.get
                liftIO $ st._stLog.lgError $ "Error deleting model: " <> show e
              )

    -- Use current model for #Temp chat
    (Just C.NModelsList, Vty.KChar 't', []) -> do
      chats <- use C.stChatsList
      case find (\i -> i.chatName == "#Temp") chats of
        Nothing -> U.setFooterMessage 8 $ "No temp chat" --TODO
        Just chat1 -> do
          B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName) >>= \case
            Nothing -> U.setFooterMessage 8 $ "No model selected" --TODO
            Just model -> do
              store <- use C.stStore
              now <- liftIO DT.getCurrentTime

              let chat2 = chat1
                    { M.chatModel = model
                    , M.chatUpdatedAt = now
                    }

              C.stTab .= C.TabChat
              C.stFocusChat %= BF.focusSetCurrent C.NChatInputEdit
              liftIO $ store.swSaveChat chat2
              liftIO . BCh.writeBChan commandChan $ C.CmdRefreshChatsList (Just . Right $ chat2.chatId) --TODO dont send message just call change chat


    -- Use current model for a new chat
    (Just C.NModelsList, Vty.KChar 'n', []) -> do
      C.stTab .= C.TabChat
      modelName <- B.gets (^?  C.stModelsList . BL.listSelectedElementL . to M.miName)
      WTct.startNewChat commandChan modelName


    (Just C.NModelsList, Vty.KFun 5, []) -> do
      liftIO $ BCh.writeBChan commandChan C.CmdRefreshModelList

    (Just C.NModelsList, _, _) -> do
      B.zoom C.stModelsList $ BL.handleListEventVi BL.handleListEvent ve

    _ -> pass


handleAppEventGotModelList :: BCh.BChan C.Command -> [O.ModelInfo] -> B.EventM C.Name C.UiState ()
handleAppEventGotModelList commandChan ms1 = do
  cfg <- use C.stAppConfig
  prevList <- use C.stModelsList

  let ms2 = ms1
  let ms = ms2 <&> \m -> M.ModelItem
        { miName = m.name
        , miInfo = m
        , miShow = Nothing
        , miTag = fromMaybe "" $ cfg.acModelTag ^. at m.name
        }
      prevSelected = snd <$> BL.listSelectedElement prevList
      prevSelectedName = (.miName) <$> prevSelected

  C.stModels .= ms
  filteredModels <- filterModels
  let ix = findIndex (\x -> Just x.miName == prevSelectedName) filteredModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix
  C.stModelListLoading .= False

  C.stModelShowLoading .= True
  liftIO . BCh.writeBChan commandChan $ C.CmdRefreshModelShow (ms <&> M.miName)



handleAppEventGotModelShow :: BCh.BChan C.Command -> (Text, O.ShowModelResponse) -> B.EventM C.Name C.UiState ()
handleAppEventGotModelShow _commandChan (m, s) = do
  l1 <- use C.stModelsList
  vs1 <- use C.stModels

  let vs2 = vs1 <&> \old ->
       if old.miName == m
         then old { M.miShow = Just s }
         else old

      selected = snd <$> BL.listSelectedElement l1
      selectedName = (.miName) <$> selected

  C.stModels .= vs2
  filteredModels <- filterModels

  let ix = findIndex (\x -> Just x.miName == selectedName) filteredModels
  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix


handleAppEventModelShowDone :: BCh.BChan C.Command -> B.EventM C.Name C.UiState ()
handleAppEventModelShowDone _commandChan = do
  l1 <- use C.stModelsList
  filteredModels <- filterModels

  let
    selected = snd <$> BL.listSelectedElement l1
    selectedName = (.miName) <$> selected
    ix = findIndex (\x -> Just x.miName == selectedName) filteredModels

  C.stModelsList %= BL.listReplace (V.fromList filteredModels) ix
  C.stModelShowLoading .= False


refreshModelsList :: BCh.BChan C.UiEvent -> IO ()
refreshModelsList eventChan = do
  mis' <- O.list
  let ms = maybe [] (\(O.Models x) -> x) mis'
  BCh.writeBChan eventChan . C.UeGotModelList $ ms


refreshModelsShow :: C.AppConfig -> [Text] -> BCh.BChan C.UiEvent -> IO ()
refreshModelsShow cfg names eventChan = do
  forConcurrently_ names $ \n -> do
    s' <- O.showModelOps (Just $ fromMaybe C.ollamaDefaultUrl cfg.acOllamaUrl) n Nothing
    case s' of
      Nothing -> pass
      Just s -> (BCh.writeBChan eventChan $ C.UeGotModelShow (n, s))

  BCh.writeBChan eventChan $ C.UeModelShowDone
----------------------------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------------------------------
filterModels :: B.EventM C.Name C.UiState [M.ModelItem]
filterModels = do
  vs <- use C.stModels
  t <- Txt.strip . Txt.toLower <$> use C.stModelsFilter
  let vs2 =
       if Txt.null t
        then vs
        else
          filter (\mi ->
            let
              name = Txt.strip . Txt.toLower $ mi.miName
              capabilities = Txt.toLower . Txt.intercalate " " . fromMaybe [] . join $ mi.miShow <&> (.capabilities)
              tags = Txt.toLower mi.miTag
            in
            Txt.isInfixOf t (name <> " " <> capabilities <> " " <> tags)
          )
          vs

  pure . reverse $ sortOn (\x ->
                            case x.miShow of
                              Nothing -> 0.0
                              Just s -> fromMaybe 0 $ U.parseParams s.details
                          )
                          vs2
---------------------------------------------------------------------------------------------------
