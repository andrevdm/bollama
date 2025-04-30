{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module App where

import           Verset

import Brick ((<=>), (<+>))
import Brick.AttrMap qualified as BA
import Brick.BChan qualified as BCh
import Brick.Focus qualified as BF
import Brick qualified as B
--import Brick.Widgets.Border qualified as BB
--import Brick.Widgets.Border.Style qualified as BBS
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar qualified as TV
import Control.Lens (makeLenses)
import Control.Lens ((^.), (^?), (%=), (.=), (%~), use, to, at, _Just)
import Data.Aeson qualified as Ae
import Data.List (findIndex)
import Data.Text qualified as Txt
import Data.Text.IO qualified as Txt
import Data.Text.Zipper qualified as TxtZ
import Data.Time qualified as DT
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Version qualified as Ver
import Graphics.Vty.CrossPlatform qualified as Vty
import Graphics.Vty qualified as Vty
import Ollama qualified as O
import Paths_bollama qualified as Paths
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as Fp
import Text.Printf (printf)

data Name
  = NListModels
  | NListPs
  | NEditModelSearch
  | NEditModelTag
  deriving stock (Show, Eq, Ord)

data UiEvent
  = UeTick !Int
  | UeGotTime !DT.UTCTime
  | UeGotModelList ![O.ModelInfo]
  | UeGotModelShow !(Text, O.ShowModelResponse)
  | UeModelShowDone
  | UePsList ![O.RunningModel]
  deriving stock (Show, Eq)


data Command
  = CmdRefreshModelList
  | CmdRefreshModelShow ![Text]
  | CmdRefreshPs
  deriving stock (Show, Eq, Ord)


data UiState = UiState
  { _stTick :: !Int
  , _stFocusModels :: !(BF.FocusRing Name)
  , _stFocusPs :: !(BF.FocusRing Name)
  , _stFocusChat :: !(BF.FocusRing Name)
  , _stModels :: ![ModelItem]
  , _stModelsList :: !(BL.List Name ModelItem)
  , _stModelsFilter :: !Text
  , _stModelListLoading :: !Bool
  , _stModelShowLoading :: !Bool
  , _stPs :: !(BL.List Name O.RunningModel)
  , _stLoadingPs :: !Bool
  , _stTime :: !DT.UTCTime
  , _stTab :: !Tab
  , _stNow :: !DT.UTCTime
  , _stDebug :: !Text
  , _stFooterWidget :: !(Maybe (Name, UiState -> B.Widget Name))
  , _stModelFilterEditor :: !(BE.Editor Text Name)
  , _stModelTagEditor :: !(BE.Editor Text Name)
  , _stAppConfig :: !AppConfig
  }

data ModelItem = ModelItem
  { miName :: !Text
  , miInfo :: !O.ModelInfo
  , miShow :: !(Maybe O.ShowModelResponse)
  , miTag :: !Text
  }

data Tab
  = TabModels
  | TabPs
  | TabChat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data AppConfig = AppConfig
  { acModelTag :: !(Map Text Text)
  } deriving (Show, Eq, Generic)

makeLenses ''UiState


runTui :: IO ()
runTui = do
  eventChan <- BCh.newBChan 50

  commandChan <- BCh.newBChan @Command 50
  void . forkIO $ runCommands commandChan eventChan
  void . forkIO $ runTick eventChan

  let app = B.App {
      B.appDraw = drawUI
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = handleEvent commandChan
    , B.appAttrMap = const attrMap
    , B.appStartEvent = liftIO $ do
       BCh.writeBChan commandChan CmdRefreshModelList
    }


  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty

  now <- DT.getCurrentTime

  cfg <- loadAppConfig

  let initialState = UiState
       { _stTick = 0
       , _stFocusModels = BF.focusRing [NListModels]
       , _stFocusPs = BF.focusRing [NListPs]
       , _stFocusChat = BF.focusRing []
       , _stModels = []
       , _stModelsList = BL.list NListModels mempty 1
       , _stModelsFilter = ""
       , _stModelListLoading = True
       , _stModelShowLoading = True
       , _stTime = now
       , _stTab = TabModels
       , _stPs = BL.list NListPs mempty 1
       , _stLoadingPs = True
       , _stNow = now
       , _stDebug = ""
       , _stFooterWidget = Nothing
       , _stModelFilterEditor = BE.editorText NEditModelSearch (Just 1) ""
       , _stModelTagEditor = BE.editorText NEditModelTag (Just 1) ""
       , _stAppConfig = cfg
       }

  _finalState <- B.customMain @Name initialVty buildVty (Just eventChan) app initialState
  pass



drawUI :: UiState -> [B.Widget Name]
drawUI st =
  [contentBlock' <=> B.fill ' ' <=> footer]


  where
    contentBlock' = drawTabs

    footer =
      B.vLimit 1 . B.withAttr (B.attrName "footer") $ B.txt (verText <> " | ") <+> drawFooterInput <+> B.fill ' '

    drawFooterInput =
      case st._stFooterWidget of
        Nothing -> B.txt st._stDebug
        Just (_name', fwidget) -> fwidget st

    drawTabs =
      drawTabHeader
      <=>
      (B.padBottom (B.Pad 1) . B.vLimit 1 . B.withAttr (B.attrName "tabSelected") $ B.fill ' ')
      <=>
      drawTabsContent

    drawTabHeader =
      let ts :: [Tab] = [minBound .. maxBound] in
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
        TabModels -> drawModelsInner
        TabPs     -> drawPsInner
        TabChat   -> B.txt "TODO chat"


    drawPsInner =
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
      then spinner
      else BL.renderList (\_ p -> renderPsListItem p) False st._stPs


    renderPsListItem p =
      let
        age1 = DT.diffUTCTime p.expiresAt st._stNow
        age2 = timeSpanToHuman age1
      in
      B.vLimit 1 $ B.hBox [
          colTb col  70 p.modelName ""
        , colTe col  11 (bytesToGb p.size_) ""
        , colTe col  11 (bytesToGb p.sizeVRam) ""
        , colTe col  40 age2 ""
        ]


    drawModelsInner =
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
      then spinner
      else BL.renderList (\_ e -> renderModelListItem e) False (st._stModelsList)


    renderModelListItem :: ModelItem -> B.Widget Name
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
        , colTe col  11 (maybe spinnerText (.details.parameterSize) sm) ""
        , colTe col  11 (maybe "" (.details.quantizationLevel) sm) ""
        , colTe col  11 (((.modelInfo.llamaContextLength) <$> sm) & join & maybe "" show) ""
        , colTb col  11 (bytesToGb mi.size) ""
        , colTe col  17 (maybe "" (.details.familiy) sm) ""
        , colTe col  40 cs ""
        , colTe col  50 usr ""
        ]

    colTe colF width txt' attr =
      if (Txt.length txt' > width)
        then colF width (Txt.take (width - 3) txt' <> "...") attr
        else colF width txt' attr

    colTb colF width txt' attr =
      if (Txt.length txt' > width)
        then colF width ("..." <> Txt.take (width - 3) txt') attr
        else colF width txt' attr

    col width txt' attr =
      (B.vLimit 1 . B.hLimit width $ (B.withAttr (BA.attrName attr) $ B.txt txt') <+> B.fill ' ')

    col' width txt' attr =
      (B.vLimit 1 . B.hLimit width $ B.fill ' ' <+> (B.withAttr (BA.attrName attr) $ B.txt txt'))

    spinnerText =
      fromMaybe "" $ spinnerFrames `atMay` ((st ^. stTick + 0) `mod` length spinnerFrames)

    spinner =
      B.withAttr (B.attrName "colHeader") . B.txt $ spinnerText



handleEvent :: BCh.BChan Command -> B.BrickEvent Name UiEvent -> B.EventM Name UiState ()
handleEvent commandChan ev = do
  case ev of
    B.AppEvent (UeTick t) -> do
      stTick .= t
      currentTab <- use stTab

      when (t `mod` 50 == 0 && currentTab == TabPs) $ do
        liftIO (BCh.writeBChan commandChan CmdRefreshPs)

    B.AppEvent (UeGotModelList ms1) -> do
      cfg <- use stAppConfig
      let ms2 = ms1
      let ms = ms2 <&> \m -> ModelItem
            { miName = m.name
            , miInfo = m
            , miShow = Nothing
            , miTag = fromMaybe "" $ cfg.acModelTag ^. at m.name
            }

      stModels .= ms
      filteredModels <- filterModels
      stModelsList %= BL.listReplace (V.fromList filteredModels) Nothing
      stModelListLoading .= False

      stModelShowLoading .= True
      liftIO . BCh.writeBChan commandChan $ CmdRefreshModelShow (ms <&> miName)

    B.AppEvent (UePsList ps') -> do
      let ps = sortOn (.modelName) ps'
      psl <- use stPs
      let wasSelected = fromMaybe "" $ (.modelName) . snd <$> BL.listSelectedElement psl
      stPs .= BL.listFindBy (\i -> i.modelName == wasSelected) (BL.list NListModels (V.fromList ps) 1)
      stLoadingPs .= False

    B.AppEvent (UeGotModelShow (m, s)) -> do
      vs1 <- use stModels
      let
        ix = findIndex (\x -> x.miName == m) vs1
        vs2 = vs1 <&> \old ->
          if old.miName == m
            then old { miShow = Just s }
            else old

      stModels .= vs2
      filteredModels <- filterModels
      stModelsList %= BL.listReplace (V.fromList filteredModels) ix

    B.AppEvent UeModelShowDone -> do
      l1 <- use stModelsList
      let
        selected = snd <$> BL.listSelectedElement l1
        selectedName = (.miName) <$> selected
        vs1 = V.toList $ BL.listElements l1
        vs2 = reverse $ sortOn parseParams vs1
        ix = findIndex (\x -> Just x.miName == selectedName) vs2

      stModels .= vs2
      filteredModels <- filterModels
      stModelsList %= BL.listReplace (V.fromList filteredModels) ix
      stModelShowLoading .= False

    B.AppEvent (UeGotTime t) -> do
      stTime .= t

    B.VtyEvent ve@(Vty.EvKey k ms) -> do
      st <- B.get
      let
        footerWidgetName = fst <$> st._stFooterWidget
        focused =
          case st._stTab of
            TabModels -> BF.focusGetCurrent st._stFocusModels
            TabPs     -> BF.focusGetCurrent st._stFocusPs
            TabChat   -> BF.focusGetCurrent st._stFocusChat


      case (footerWidgetName, st._stTab, focused, k, ms) of
        ---------------------------------------------------------------------------------------------------
        -- Global
        ---------------------------------------------------------------------------------------------------
        (_, _, _, Vty.KChar 'q', [Vty.MCtrl]) -> B.halt
        ---------------------------------------------------------------------------------------------------


        ---------------------------------------------------------------------------------------------------
        -- NEditModelSearch
        ---------------------------------------------------------------------------------------------------
        (Just NEditModelSearch, _, _, Vty.KEsc, []) -> do
          stFooterWidget .= Nothing

        (Just NEditModelSearch, _, _, Vty.KEnter, []) -> do
          txt <- use (stModelFilterEditor . BE.editContentsL . to TxtZ.getText)
          stModelsFilter .= Txt.unlines txt
          stFooterWidget .= Nothing
          filteredModels <- filterModels
          wasSelected <- B.gets (^?  stModelsList . BL.listSelectedElementL . to miName)
          let ix = findIndex (\x -> Just x.miName == wasSelected) filteredModels
          stModelsList %= BL.listReplace (V.fromList filteredModels) ix

        (Just NEditModelSearch, _, _, _, _) -> do
          B.zoom stModelFilterEditor $ BE.handleEditorEvent ev
        ---------------------------------------------------------------------------------------------------



        ---------------------------------------------------------------------------------------------------
        -- NEditModelTag
        ---------------------------------------------------------------------------------------------------
        (Just NEditModelTag, _, _, Vty.KEsc, []) -> do
          stFooterWidget .= Nothing

        (Just NEditModelTag, _, _, Vty.KEnter, []) -> do
          B.gets (^?  stModelsList . BL.listSelectedElementL . to miName) >>= \case
            Nothing -> pass
            Just selected -> do
              txt <- use (stModelTagEditor . BE.editContentsL . to TxtZ.getText)
              stAppConfig %= \cfg ->
                cfg { acModelTag = Map.insert selected (Txt.unlines txt) cfg.acModelTag }
              liftIO . writeAppConfig =<< use stAppConfig

          stModelTagEditor . BE.editContentsL %= TxtZ.clearZipper
          stFooterWidget .= Nothing

        (Just NEditModelTag, _, _, _, _) -> do
          B.zoom stModelTagEditor $ BE.handleEditorEvent ev
        ---------------------------------------------------------------------------------------------------



        ---------------------------------------------------------------------------------------------------
        -- Function keys
        ---------------------------------------------------------------------------------------------------
        (_, _, _, Vty.KFun 2, []) -> do
          unless (st._stTab == TabModels) $ do
            stLoadingPs .= True
            stTab .= TabModels

        (_, _, _, Vty.KFun 3, []) -> do
          unless (st._stTab == TabPs) $ do
            liftIO . BCh.writeBChan commandChan $ CmdRefreshPs
            stLoadingPs .= True
            stTab .= TabPs

        (_, _, _, Vty.KFun 4, []) -> do
          unless (st._stTab == TabChat) $ do
            stLoadingPs .= True
            stTab .= TabChat
        ---------------------------------------------------------------------------------------------------



        ---------------------------------------------------------------------------------------------------
        -- NListModels
        ---------------------------------------------------------------------------------------------------
        (_, _, Just NListModels, Vty.KChar '/', []) -> do
          stFooterWidget .= Just (NEditModelSearch, \st2 -> (B.withAttr (B.attrName "footerTitle") $ B.txt "filter: ") <+> BE.renderEditor (B.txt . Txt.unlines) True st2._stModelFilterEditor)

        (_, _, Just NListModels, Vty.KChar 't', []) -> do
          cfg <- use stAppConfig
          tags <-
            B.gets (^?  stModelsList . BL.listSelectedElementL . to miName) >>= \case
              Nothing -> pure ""
              Just selected -> do
                pure . fromMaybe "" $ Map.lookup selected cfg.acModelTag

          stModelTagEditor .= BE.editorText NEditModelTag (Just 1) tags
          stFooterWidget .= Just (NEditModelTag, \st2 -> (B.withAttr (B.attrName "footerTitle") $ B.txt "tag: ") <+> BE.renderEditor (B.txt . Txt.unlines) True st2._stModelTagEditor)

        (_, _, Just NListModels, _, _) -> do
          B.zoom stModelsList $ BL.handleListEventVi BL.handleListEvent ve
        ---------------------------------------------------------------------------------------------------



        ---------------------------------------------------------------------------------------------------
        -- NListPs
        ---------------------------------------------------------------------------------------------------
        (_, _, Just NListPs, Vty.KChar 's', []) -> do
          B.gets (^?  stPs . BL.listSelectedElementL . to (.modelName)) >>= \case
            Nothing -> pass
            Just name -> do
              _ <- liftIO . O.generate $ O.GenerateOps
                { modelName = name
                , keepAlive = Just "0"
                , prompt = ""
                , suffix = Nothing
                , images = Nothing
                , format = Nothing
                , system = Nothing
                , template = Nothing
                , stream = Nothing
                , raw = Nothing
                , hostUrl = Just ollamaUrl
                , responseTimeOut = Nothing
                , options = Nothing
                }
              stDebug .= "stopping " <> name

          pass

        (_, _, Just NListPs, _, _) -> do
          B.zoom stPs $ BL.handleListEventVi BL.handleListEvent ve
        ---------------------------------------------------------------------------------------------------

        _ -> pass

    _ -> pass


filterModels :: B.EventM Name UiState [ModelItem]
filterModels = do
  vs <- use stModels
  t <- Txt.strip . Txt.toLower <$> use stModelsFilter

  if Txt.null t
    then pure vs
    else
      pure $ filter (\mi ->
          let
            name = Txt.strip . Txt.toLower $ mi.miName
            capabilities = Txt.toLower . Txt.intercalate " " . fromMaybe [] . join $ mi.miShow <&> (.capabilities)
            tags = Txt.toLower mi.miTag
          in
          Txt.isInfixOf t (name <> " " <> capabilities <> " " <> tags)
        )
        vs


----------------------------------------------------------------------------------------------------------------------
-- background thread
----------------------------------------------------------------------------------------------------------------------
runCommands :: BCh.BChan Command -> BCh.BChan UiEvent -> IO ()
runCommands commandChan eventChan = forever $ do
  BCh.readBChan commandChan >>= \case
    CmdRefreshModelList -> do
      mis' <- O.list
      let ms = maybe [] (\(O.Models x) -> x) mis'
      BCh.writeBChan eventChan . UeGotModelList $ ms


    CmdRefreshModelShow names -> do
      forConcurrently_ names $ \n -> do
        s' <- O.showModelOps (Just ollamaUrl) n Nothing
        case s' of
          Nothing -> pass
          Just s -> (BCh.writeBChan eventChan $ UeGotModelShow (n, s))

      BCh.writeBChan eventChan $ UeModelShowDone


    CmdRefreshPs -> do
      ps' <- O.psOps (Just ollamaUrl)
      let ps = maybe [] (\(O.RunningModels x) -> x) ps'
      BCh.writeBChan eventChan . UePsList $ ps



runTick :: BCh.BChan UiEvent -> IO ()
runTick eventChan = do
  tick' <- TV.newTVarIO (0 :: Int)

  forever $ do
    tick <- atomically . TV.stateTVar tick' $ \t ->
      let t2 = t + 1 `mod` 1_000_000 in
      (t2, t2)

    BCh.writeBChan eventChan $ UeTick tick

    when (tick `mod` 10 == 0) $ do
      now <- DT.getCurrentTime
      BCh.writeBChan eventChan $ UeGotTime now

    threadDelay 100_000
----------------------------------------------------------------------------------------------------------------------


parseParams :: ModelItem -> Maybe Double
parseParams mi =
  let
    sz = mi.miInfo.details.parameterSize
    (mul, drop') =
      if | Txt.isSuffixOf "B" sz -> (1_000_000_000.0, 1)
         | Txt.isSuffixOf "M" sz -> (1_000_000.0, 1)
         | otherwise -> (1, 0)

    sz2 = Txt.dropEnd drop' sz
  in
  case readMaybe @Double . Txt.unpack $ sz2 of
    Nothing -> Nothing
    Just sz3 -> Just $ sz3 * mul



tabName :: Tab -> Text
tabName TabModels = "F2: Models"
tabName TabPs = "F3: Running"
tabName TabChat = "F4: Chat"



bytesToHuman :: Int64 -> Text
bytesToHuman bytes' =
  Txt.pack . format . fromIntegral $ bytes'

  where
    format :: Double -> [Char]
    format bytes
      | bytes < 0  = "-" <> format (-bytes)
      | bytes < kb = printf "%.0f B" bytes
      | bytes < mb = printf "%.1f KB" (bytes / 1024)
      | bytes < gb = printf "%.1f MB" (bytes / mb)
      | bytes < tb = printf "%.1f GB" (bytes / gb)
      | otherwise  = printf "%.1f TB" (bytes / tb)

    kb = 1024
    mb = kb * 1024
    gb = mb * 1024
    tb = gb * 1024


bytesToGb :: Int64 -> Text
bytesToGb bytes' =
  let gb = fromIntegral @_ @Double bytes' / (1024 * 1024 * 1024) in
  Txt.pack . printf "%.2f GB" $ gb


timeSpanToHuman :: NominalDiffTime -> Text
timeSpanToHuman t =
  let
    secondsInMinute = 60
    secondsInHour = 60 * secondsInMinute
    secondsInDay = 24 * secondsInHour
    secondsInMonth = 30 * secondsInDay  -- Assuming 30 days in a month
    secondsInYear = 365 * secondsInDay  -- Assuming 365 days in a year

    (years, rem1) = (round @_ @Int . realToFrac @_ @Double $ t) `divMod` secondsInYear
    (months, rem2) = rem1 `divMod` secondsInMonth
    (days, rem3) = rem2 `divMod` secondsInDay
    (hours, rem4) = rem3 `divMod` secondsInHour
    (minutes, seconds) = rem4 `divMod` secondsInMinute
    fmt unit value = show value <> " " <> unit <> (if value == 1 then "" else "s")

  in
  if | years > 0 -> fmt "year" years
     | months > 0 -> fmt "month" months
     | days > 0 -> fmt "day" days
     | hours > 0 -> fmt "hour" hours
     | minutes > 0 -> fmt "minute" minutes
     | otherwise -> fmt "second" seconds


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
      (BE.editAttr             , Vty.black `B.on` Vty.cyan)
    , (BE.editFocusedAttr      , Vty.black `B.on` Vty.yellow)
    , (BL.listSelectedAttr     , B.fg Vty.yellow)
    , (B.attrName "infoTitle"  , B.fg Vty.cyan)
    , (B.attrName "time"       , B.fg Vty.yellow)
    , (B.attrName "colHeader"  , Vty.withStyle (B.fg Vty.blue) Vty.bold)
    , (B.attrName "titleText"  , B.fg Vty.green)
    , (B.attrName "normalText" , B.fg Vty.white)
    , (B.attrName "normalText2", B.fg orange)

    , (B.attrName "footer"            , Vty.black `B.on` grey)
    , (B.attrName "footerTitle"       , Vty.white `B.on` Vty.black)
    , (B.attrName "footerMessage"     , Vty.black `B.on` grey)
    , (B.attrName "version"           , Vty.yellow `B.on` grey)

    , (B.attrName "msgInfo"           , Vty.black `B.on` Vty.blue)
    , (B.attrName "msgError"          , Vty.yellow `B.on` Vty.blue)

    , (B.attrName "tabSelected"     , Vty.black `B.on` Vty.green)
    , (B.attrName "tabUnselected"   , Vty.black `B.on` Vty.blue)
    ]


ollamaUrl :: Text
--ollamaUrl = "http://localhost:11435"
ollamaUrl = "http://localhost:11434"


spinnerFrames :: [Text]
spinnerFrames = ["⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏"]
--spinnerFrames = ["-", "\\", "|", "/"]
--spinnerFrames = ["◜", "◠", "◝", "◞", "◡", "◟"]


verText :: Text
verText = Txt.pack $ Ver.showVersion Paths.version


getConfigDir :: IO FilePath
getConfigDir = do
  p <- Dir.getXdgDirectory Dir.XdgConfig "bollama"
  Dir.createDirectoryIfMissing True p
  pure p



emptyAppConfig :: AppConfig
emptyAppConfig = AppConfig
  { acModelTag = mempty
  }


instance Ae.FromJSON AppConfig where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }

instance Ae.ToJSON AppConfig where
  toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = renSnake 2 }


loadAppConfig :: IO AppConfig
loadAppConfig = do
  configDir <- getConfigDir
  let configFile = configDir </> "config.json"

  Dir.doesFileExist configFile >>= \case
    False -> pure emptyAppConfig
    True -> do
      cfg <- Ae.eitherDecodeFileStrict' configFile
      case cfg of
        Right cfg' -> pure cfg'
        Left _err -> do
          Dir.removeFile configFile
          pure $ AppConfig mempty


writeAppConfig :: AppConfig -> IO ()
writeAppConfig cfg = do
  configDir <- getConfigDir
  Dir.createDirectoryIfMissing True configDir

  let configFile = configDir </> "config.json"
  Ae.encodeFile configFile cfg


renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d

