{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Utils where

import           Verset

import Brick qualified as B
import Brick.AttrMap qualified as BA
import Brick.Forms qualified as BFm
import Brick.Widgets.Dialog qualified as BD
import Brick.Widgets.ProgressBar qualified as BP
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Data.Map.Strict qualified as Map
import Data.List (findIndex)
import Data.Text.IO qualified as Txt
import Data.Text qualified as Txt
import Data.UUID.V4 qualified as UU
import Graphics.Vty qualified as Vty
import Ollama qualified as O
import Text.Printf (printf)
import Text.RawString.QQ (r)


import Core qualified as C


parseParams :: C.ModelItem -> Maybe Double
parseParams mi =
  let sz = mi.miInfo.details.parameterSize
      (mul, drop') =
        if | Txt.isSuffixOf "B" sz -> (1_000_000_000.0, 1)
           | Txt.isSuffixOf "M" sz -> (1_000_000.0, 1)
           | otherwise -> (1, 0)

      sz2 = Txt.dropEnd drop' sz
  in
  case readMaybe @Double . Txt.unpack $ sz2 of
    Nothing -> Nothing
    Just sz3 -> Just $ sz3 * mul



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
  let secondsInMinute = 60
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


stopModel :: Text -> IO ()
stopModel name = do
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
                , hostUrl = Just C.ollamaUrl
                , responseTimeOut = Nothing
                , options = Nothing
                }
  pass


newUuidText :: IO Text
newUuidText = do
  uuid <- UU.nextRandom
  pure $ show uuid


logLevelFilters :: [[C.LogLevel]]
logLevelFilters =
  [ [C.LlError]
  , [C.LlWarn, C.LlError]
  , [C.LlInfo, C.LlWarn, C.LlError]
  , [C.LlDebug, C.LlInfo, C.LlWarn, C.LlError]
  ]

logLevelName :: C.LogLevel -> Text
logLevelName C.LlDebug = "DEBUG"
logLevelName C.LlInfo = "INFO"
logLevelName C.LlWarn = "WARN"
logLevelName C.LlError = "ERROR"
logLevelName C.LlCritical = "CRITICAL"



removeThink :: Text -> Text
removeThink str =
  if not (Txt.isPrefixOf "<think>" str)
  then str
  else
    let ls = drop 1 . Txt.lines $ str in
    case findIndex (== "</think>") ls of
      Nothing -> str
      Just i -> Txt.strip . Txt.unlines . drop (i + 1) $ ls


attrMapFromFile :: FilePath -> IO ([Text], BA.AttrMap)
attrMapFromFile file = do
  txt <- Txt.readFile file
  attrMapFromText txt

attrMapFromText :: Text -> IO ([Text], BA.AttrMap)
attrMapFromText t = do
  -- Read the default theme
  (_, defTheme) <- attrMapFromText' defaultTheme
  -- Read user theme
  (errors, attrsUser) <- attrMapFromText' t

  let
      -- Add all known colours as _fg_ and _bg_ attributes
      knownAsFs = knownColours <&> \ (n, c) -> (B.attrName . Txt.unpack $ "_fg_" <> n, B.fg c)
      knownAsBs = knownColours <&> \ (n, c) -> (B.attrName . Txt.unpack $ "_bg_" <> n, B.bg c)

      -- Combine parse attributes, with defaults and colours
      attrs = Map.toList $ Map.unions [ Map.fromList attrsUser
                                      , Map.fromList defTheme
                                      , Map.fromList $ knownAsBs <> knownAsFs
                                      ]

      defAttr = snd <$> find (\(n, _) -> n == B.attrName "default") attrs
      attrMap = BA.attrMap (fromMaybe Vty.defAttr defAttr) attrs

  pure (errors, attrMap)


attrMapFromText' :: Text -> IO ([Text], [(B.AttrName, Vty.Attr)])
attrMapFromText' txt = do
  let ls1 = Txt.lines txt
      ls2 = Txt.strip <$> ls1
      ls3 = filter (not . Txt.isInfixOf "--") ls2
      ls4 = filter (not . Txt.isInfixOf "//") ls3
      ls = filter (not . Txt.null) ls4
      lcs = Txt.splitOn "," <$> ls
      clrs = Map.fromList knownColours
      lines = parseLine clrs <$> lcs
      errors = lefts lines
      attrs1 = rights lines
  pure (errors, attrs1)

  where
    parseLine :: Map Text Vty.Color -> [Text] -> Either Text (BA.AttrName, Vty.Attr)
    parseLine clrs [n, f] = parseLine clrs [n, f, "-", "-"]
    parseLine clrs [n, f, b] = parseLine clrs [n, f, b, "-"]
    parseLine clrs [n1, f1, b1, s1] = do
      f2 <- readColour clrs . Txt.strip $ f1
      b2 <- readColour clrs . Txt.strip $ b1
      s2 <- readStyle . Txt.strip $ s1
      let n2 = readAttrName . Txt.strip $ n1
          s3 = fromMaybe Vty.defaultStyleMask s2

      case (f2, b2) of
        (Just f, Just b) -> pure (n2, Vty.withStyle (f `B.on` b) s3)
        (Just f, Nothing) -> pure (n2, Vty.withStyle (B.fg f) s3)
        (Nothing, Just b) -> pure (n2, Vty.withStyle (B.bg b) s3)
        (Nothing, Nothing) -> pure (n2, Vty.withStyle Vty.defAttr s3)

    parseLine _ vs = Left $ "Invalid line: " <> Txt.intercalate "," vs


    readColour :: Map Text Vty.Color -> Text -> Either Text (Maybe Vty.Color)
    readColour _ "-" = Right Nothing
    readColour clrs t = do
      if Txt.isPrefixOf "#" t
        then do
          let clr = do
                a <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 1 $ t)
                b <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 3 $ t)
                c <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 5 $ t)
                pure $ Vty.linearColor @Int a b c
          case  clr of
            Nothing -> Left $ "Unknown color: " <> t
            Just c -> Right $ Just c

        else do
          case Map.lookup t clrs of
            Nothing -> Left $ "Unknown color: " <> t
            Just c -> Right $ Just c


    readStyle :: Text -> Either Text (Maybe Vty.Style)
    readStyle "standout" = Right $ Just Vty.standout
    readStyle "underline" = Right $ Just Vty.underline
    readStyle "reverseVideo" = Right $ Just Vty.reverseVideo
    readStyle "blink" = Right $ Just Vty.blink
    readStyle "dim" = Right $ Just Vty.dim
    readStyle "bold" = Right $ Just Vty.bold
    readStyle "bright" = Right $ Just Vty.bold
    readStyle "italic" = Right $ Just Vty.italic
    readStyle "strikethrough" = Right $ Just Vty.strikethrough
    readStyle "-" = Right $ Just Vty.defaultStyleMask
    readStyle "_" = Right $ Just Vty.defaultStyleMask
    readStyle "" = Right $ Just Vty.defaultStyleMask
    readStyle x = Left $ "Unknown style: " <> x


    readAttrName :: Text -> BA.AttrName
    readAttrName "editAttr" = BE.editAttr
    readAttrName "editFocusedAttr" = BE.editAttr
    readAttrName "scrollbarAttr" = B.scrollbarAttr
    readAttrName "scrollbarTroughAttr" = B.scrollbarTroughAttr
    readAttrName "scrollbarHandleAttr" = B.scrollbarHandleAttr
    readAttrName "dialogAttr" = BD.dialogAttr
    readAttrName "buttonAttr" = BD.buttonAttr
    readAttrName "buttonSelectedAttr" = BD.buttonSelectedAttr
    readAttrName "listAttr" = BL.listAttr
    readAttrName "listSelectedAttr" = BL.listSelectedAttr
    readAttrName "listSelectedFocusedAttr" = BL.listSelectedFocusedAttr
    readAttrName "progressCompleteAttr" = BP.progressCompleteAttr
    readAttrName "progressIncompleteAttr" = BP.progressCompleteAttr
    readAttrName "focusedFormInputAttr" = BFm.focusedFormInputAttr
    readAttrName "invalidFormInputAttr" = BFm.invalidFormInputAttr
    readAttrName t = BA.attrName . Txt.unpack $ t


defaultTheme :: Text
defaultTheme = [r|
    --default                    , red                   ,  blue
    borderSelectedLabel        , violet                ,  -
    chatMsgA                   , black                 ,  wheat4
    chatMsgB                   , black                 ,  tan
    chatMsgSelected            , black                 ,  -
    chatDefaultMarker          , yellow                ,  -
    colHeader                  , deep_sky_blue2        ,  -               , bold
    editAttr                   , black                 ,  grey
    editFocusedAttr            , black                 ,  sky_blue1
    footer                     , black                 ,  grey
    footerMessage              , black                 ,  grey
    footerTitle                , white                 ,  black
    infoTitle                  , cyan                  ,  -
    listAttr                   , white                 ,  grey7
    listSelectedAttr           , cornflower_blue       ,  -
    listSelectedFocusedAttr    , black                 ,  cornflower_blue
    msgError                   , red                   ,  -
    msgInfo                    , black                 ,  blue
    spinner1                   , light_sky_blue1       ,  -
    spinner2                   , deep_pink3            ,  -
    tabFooter                  , black                 ,  grey
    tabSelected                , black                 ,  cornflower_blue
    tabUnselected              , black                 ,  grey
    time                       , yellow                ,  -
    version                    , yellow                ,  grey

    popup                      , black                 ,  pale_turquoise4
    popupHeader                , blue3                 ,  pale_turquoise4
    popupButtonOk              , green                 ,  black
    popupButtonOkFocused       , black                 ,  green
    popupButtonCancel          , red                   ,  black
    popupButtonDisabled        , black                 ,  grey35
    popupButtonDisabledFocused , black                 ,  grey
    popupButtonCancelFocused   , black                 ,  red
    popupTableHeader           , deep_pink4            ,  pale_turquoise4

    popupError                 , black                 ,  red3
    popupErrorText             , black                 ,  red3

    invalidFormInputAttr       , black                 ,  red
|]



knownColours :: [(Text, Vty.Color)]
knownColours =
    [ ("black",               Vty.black)
    , ("red",                 Vty.red)
    , ("green",               Vty.green)
    , ("yellow",              Vty.yellow)
    , ("blue",                Vty.blue)
    , ("magenta",             Vty.magenta)
    , ("cyan",                Vty.cyan)
    , ("white",               Vty.white)
    , ("brightBlack",         Vty.brightBlack)
    , ("brightRed",           Vty.brightRed)
    , ("brightGreen",         Vty.brightGreen)
    , ("brightYellow",        Vty.brightYellow)
    , ("brightBlue",          Vty.brightBlue)
    , ("brightMagenta",       Vty.brightMagenta)
    , ("brightCyan",          Vty.brightCyan)
    , ("brightWhite",         Vty.brightWhite)
    , ("orange",              Vty.linearColor @Int 0xFA 0xA5 0x00)
    , ("grey",                Vty.linearColor @Int 128  128 128)
    , ("skyBlue",             Vty.linearColor @Int 0x87 0xCE 0xEB)
    , ("slateBlue",           Vty.linearColor @Int 0x6A 0x5A 0xCD)
    , ("pink",                Vty.linearColor @Int 0xFF 0xC0 0xCB)


    , ("grey0"               , Vty.linearColor @Int 0x00 0x00 0x00)
    , ("navy_blue"           , Vty.linearColor @Int 0x00 0x00 0x5f)
    , ("dark_blue"           , Vty.linearColor @Int 0x00 0x00 0x87)
    , ("blue3"               , Vty.linearColor @Int 0x00 0x00 0xd7)
    , ("blue1"               , Vty.linearColor @Int 0x00 0x00 0xff)
    , ("dark_green"          , Vty.linearColor @Int 0x00 0x5f 0x00)
    , ("deep_sky_blue4"      , Vty.linearColor @Int 0x00 0x5f 0xaf)
    , ("dodger_blue3"        , Vty.linearColor @Int 0x00 0x5f 0xd7)
    , ("dodger_blue2"        , Vty.linearColor @Int 0x00 0x5f 0xff)
    , ("green4"              , Vty.linearColor @Int 0x00 0x87 0x00)
    , ("spring_green4"       , Vty.linearColor @Int 0x00 0x87 0x5f)
    , ("turquoise4"          , Vty.linearColor @Int 0x00 0x87 0x87)
    , ("deep_sky_blue3"      , Vty.linearColor @Int 0x00 0x87 0xd7)
    , ("dodger_blue1"        , Vty.linearColor @Int 0x00 0x87 0xff)
    , ("dark_cyan"           , Vty.linearColor @Int 0x00 0xaf 0x87)
    , ("light_sea_green"     , Vty.linearColor @Int 0x00 0xaf 0xaf)
    , ("deep_sky_blue2"      , Vty.linearColor @Int 0x00 0xaf 0xd7)
    , ("deep_sky_blue1"      , Vty.linearColor @Int 0x00 0xaf 0xff)
    , ("green3"              , Vty.linearColor @Int 0x00 0xd7 0x00)
    , ("spring_green3"       , Vty.linearColor @Int 0x00 0xd7 0x5f)
    , ("cyan3"               , Vty.linearColor @Int 0x00 0xd7 0xaf)
    , ("dark_turquoise"      , Vty.linearColor @Int 0x00 0xd7 0xd7)
    , ("turquoise2"          , Vty.linearColor @Int 0x00 0xd7 0xff)
    , ("green1"              , Vty.linearColor @Int 0x00 0xff 0x00)
    , ("spring_green2"       , Vty.linearColor @Int 0x00 0xff 0x5f)
    , ("spring_green1"       , Vty.linearColor @Int 0x00 0xff 0x87)
    , ("medium_spring_green" , Vty.linearColor @Int 0x00 0xff 0xaf)
    , ("cyan2"               , Vty.linearColor @Int 0x00 0xff 0xd7)
    , ("cyan1"               , Vty.linearColor @Int 0x00 0xff 0xff)
    , ("purple4"             , Vty.linearColor @Int 0x5f 0x00 0xaf)
    , ("purple3"             , Vty.linearColor @Int 0x5f 0x00 0xd7)
    , ("blue_violet"         , Vty.linearColor @Int 0x5f 0x00 0xff)
    , ("grey37"              , Vty.linearColor @Int 0x5f 0x5f 0x5f)
    , ("medium_purple4"      , Vty.linearColor @Int 0x5f 0x5f 0x87)
    , ("slate_blue3"         , Vty.linearColor @Int 0x5f 0x5f 0xd7)
    , ("royal_blue1"         , Vty.linearColor @Int 0x5f 0x5f 0xff)
    , ("chartreuse4"         , Vty.linearColor @Int 0x5f 0x87 0x00)
    , ("pale_turquoise4"     , Vty.linearColor @Int 0x5f 0x87 0x87)
    , ("steel_blue"          , Vty.linearColor @Int 0x5f 0x87 0xaf)
    , ("steel_blue3"         , Vty.linearColor @Int 0x5f 0x87 0xd7)
    , ("cornflower_blue"     , Vty.linearColor @Int 0x5f 0x87 0xff)
    , ("dark_sea_green4"     , Vty.linearColor @Int 0x5f 0xaf 0x5f)
    , ("cadet_blue"          , Vty.linearColor @Int 0x5f 0xaf 0xaf)
    , ("sky_blue3"           , Vty.linearColor @Int 0x5f 0xaf 0xd7)
    , ("chartreuse3"         , Vty.linearColor @Int 0x5f 0xd7 0x00)
    , ("sea_green3"          , Vty.linearColor @Int 0x5f 0xd7 0x87)
    , ("aquamarine3"         , Vty.linearColor @Int 0x5f 0xd7 0xaf)
    , ("medium_turquoise"    , Vty.linearColor @Int 0x5f 0xd7 0xd7)
    , ("steel_blue1"         , Vty.linearColor @Int 0x5f 0xd7 0xff)
    , ("sea_green2"          , Vty.linearColor @Int 0x5f 0xff 0x5f)
    , ("sea_green1"          , Vty.linearColor @Int 0x5f 0xff 0xaf)
    , ("dark_slate_gray2"    , Vty.linearColor @Int 0x5f 0xff 0xff)
    , ("dark_red"            , Vty.linearColor @Int 0x87 0x00 0x00)
    , ("dark_magenta"        , Vty.linearColor @Int 0x87 0x00 0xaf)
    , ("orange4"             , Vty.linearColor @Int 0x87 0x5f 0x00)
    , ("light_pink4"         , Vty.linearColor @Int 0x87 0x5f 0x5f)
    , ("plum4"               , Vty.linearColor @Int 0x87 0x5f 0x87)
    , ("medium_purple3"      , Vty.linearColor @Int 0x87 0x5f 0xd7)
    , ("slate_blue1"         , Vty.linearColor @Int 0x87 0x5f 0xff)
    , ("wheat4"              , Vty.linearColor @Int 0x87 0x87 0x5f)
    , ("grey53"              , Vty.linearColor @Int 0x87 0x87 0x87)
    , ("light_slate_grey"    , Vty.linearColor @Int 0x87 0x87 0xaf)
    , ("medium_purple"       , Vty.linearColor @Int 0x87 0x87 0xd7)
    , ("light_slate_blue"    , Vty.linearColor @Int 0x87 0x87 0xff)
    , ("yellow4"             , Vty.linearColor @Int 0x87 0xaf 0x00)
    , ("dark_sea_green"      , Vty.linearColor @Int 0x87 0xaf 0x87)
    , ("light_sky_blue3"     , Vty.linearColor @Int 0x87 0xaf 0xd7)
    , ("sky_blue2"           , Vty.linearColor @Int 0x87 0xaf 0xff)
    , ("chartreuse2"         , Vty.linearColor @Int 0x87 0xd7 0x00)
    , ("pale_green3"         , Vty.linearColor @Int 0x87 0xd7 0x87)
    , ("dark_slate_gray3"    , Vty.linearColor @Int 0x87 0xd7 0xd7)
    , ("sky_blue1"           , Vty.linearColor @Int 0x87 0xd7 0xff)
    , ("chartreuse1"         , Vty.linearColor @Int 0x87 0xff 0x00)
    , ("light_green"         , Vty.linearColor @Int 0x87 0xff 0x87)
    , ("aquamarine1"         , Vty.linearColor @Int 0x87 0xff 0xd7)
    , ("dark_slate_gray1"    , Vty.linearColor @Int 0x87 0xff 0xff)
    , ("deep_pink4"          , Vty.linearColor @Int 0xaf 0x00 0x5f)
    , ("medium_violet_red"   , Vty.linearColor @Int 0xaf 0x00 0x87)
    , ("dark_violet"         , Vty.linearColor @Int 0xaf 0x00 0xd7)
    , ("purple"              , Vty.linearColor @Int 0xaf 0x00 0xff)
    , ("medium_orchid3"      , Vty.linearColor @Int 0xaf 0x5f 0xaf)
    , ("medium_orchid"       , Vty.linearColor @Int 0xaf 0x5f 0xd7)
    , ("dark_goldenrod"      , Vty.linearColor @Int 0xaf 0x87 0x00)
    , ("rosy_brown"          , Vty.linearColor @Int 0xaf 0x87 0x87)
    , ("grey63"              , Vty.linearColor @Int 0xaf 0x87 0xaf)
    , ("medium_purple2"      , Vty.linearColor @Int 0xaf 0x87 0xd7)
    , ("medium_purple1"      , Vty.linearColor @Int 0xaf 0x87 0xff)
    , ("dark_khaki"          , Vty.linearColor @Int 0xaf 0xaf 0x5f)
    , ("navajo_white3"       , Vty.linearColor @Int 0xaf 0xaf 0x87)
    , ("grey69"              , Vty.linearColor @Int 0xaf 0xaf 0xaf)
    , ("light_steel_blue3"   , Vty.linearColor @Int 0xaf 0xaf 0xd7)
    , ("light_steel_blue"    , Vty.linearColor @Int 0xaf 0xaf 0xff)
    , ("dark_olive_green3"   , Vty.linearColor @Int 0xaf 0xd7 0x5f)
    , ("dark_sea_green3"     , Vty.linearColor @Int 0xaf 0xd7 0x87)
    , ("light_cyan3"         , Vty.linearColor @Int 0xaf 0xd7 0xd7)
    , ("light_sky_blue1"     , Vty.linearColor @Int 0xaf 0xd7 0xff)
    , ("green_yellow"        , Vty.linearColor @Int 0xaf 0xff 0x00)
    , ("dark_olive_green2"   , Vty.linearColor @Int 0xaf 0xff 0x5f)
    , ("pale_green1"         , Vty.linearColor @Int 0xaf 0xff 0x87)
    , ("dark_sea_green2"     , Vty.linearColor @Int 0xaf 0xff 0xaf)
    , ("pale_turquoise1"     , Vty.linearColor @Int 0xaf 0xff 0xff)
    , ("red3"                , Vty.linearColor @Int 0xd7 0x00 0x00)
    , ("deep_pink3"          , Vty.linearColor @Int 0xd7 0x00 0x87)
    , ("magenta3"            , Vty.linearColor @Int 0xd7 0x00 0xd7)
    , ("dark_orange3"        , Vty.linearColor @Int 0xd7 0x5f 0x00)
    , ("indian_red"          , Vty.linearColor @Int 0xd7 0x5f 0x5f)
    , ("hot_pink3"           , Vty.linearColor @Int 0xd7 0x5f 0x87)
    , ("hot_pink2"           , Vty.linearColor @Int 0xd7 0x5f 0xaf)
    , ("orchid"              , Vty.linearColor @Int 0xd7 0x5f 0xd7)
    , ("orange3"             , Vty.linearColor @Int 0xd7 0x87 0x00)
    , ("light_salmon3"       , Vty.linearColor @Int 0xd7 0x87 0x5f)
    , ("light_pink3"         , Vty.linearColor @Int 0xd7 0x87 0x87)
    , ("pink3"               , Vty.linearColor @Int 0xd7 0x87 0xaf)
    , ("plum3"               , Vty.linearColor @Int 0xd7 0x87 0xd7)
    , ("violet"              , Vty.linearColor @Int 0xd7 0x87 0xff)
    , ("gold3"               , Vty.linearColor @Int 0xd7 0xaf 0x00)
    , ("light_goldenrod3"    , Vty.linearColor @Int 0xd7 0xaf 0x5f)
    , ("tan"                 , Vty.linearColor @Int 0xd7 0xaf 0x87)
    , ("misty_rose3"         , Vty.linearColor @Int 0xd7 0xaf 0xaf)
    , ("thistle3"            , Vty.linearColor @Int 0xd7 0xaf 0xd7)
    , ("plum2"               , Vty.linearColor @Int 0xd7 0xaf 0xff)
    , ("yellow3"             , Vty.linearColor @Int 0xd7 0xd7 0x00)
    , ("khaki3"              , Vty.linearColor @Int 0xd7 0xd7 0x5f)
    , ("light_yellow3"       , Vty.linearColor @Int 0xd7 0xd7 0xaf)
    , ("grey84"              , Vty.linearColor @Int 0xd7 0xd7 0xd7)
    , ("light_steel_blue1"   , Vty.linearColor @Int 0xd7 0xd7 0xff)
    , ("yellow2"             , Vty.linearColor @Int 0xd7 0xff 0x00)
    , ("dark_olive_green1"   , Vty.linearColor @Int 0xd7 0xff 0x87)
    , ("dark_sea_green1"     , Vty.linearColor @Int 0xd7 0xff 0xaf)
    , ("honeydew2"           , Vty.linearColor @Int 0xd7 0xff 0xd7)
    , ("light_cyan1"         , Vty.linearColor @Int 0xd7 0xff 0xff)
    , ("red1"                , Vty.linearColor @Int 0xff 0x00 0x00)
    , ("deep_pink2"          , Vty.linearColor @Int 0xff 0x00 0x5f)
    , ("deep_pink1"          , Vty.linearColor @Int 0xff 0x00 0xaf)
    , ("magenta2"            , Vty.linearColor @Int 0xff 0x00 0xd7)
    , ("magenta1"            , Vty.linearColor @Int 0xff 0x00 0xff)
    , ("orange_red1"         , Vty.linearColor @Int 0xff 0x5f 0x00)
    , ("indian_red1"         , Vty.linearColor @Int 0xff 0x5f 0x87)
    , ("hot_pink"            , Vty.linearColor @Int 0xff 0x5f 0xd7)
    , ("medium_orchid1"      , Vty.linearColor @Int 0xff 0x5f 0xff)
    , ("dark_orange"         , Vty.linearColor @Int 0xff 0x87 0x00)
    , ("salmon1"             , Vty.linearColor @Int 0xff 0x87 0x5f)
    , ("light_coral"         , Vty.linearColor @Int 0xff 0x87 0x87)
    , ("pale_violet_red1"    , Vty.linearColor @Int 0xff 0x87 0xaf)
    , ("orchid2"             , Vty.linearColor @Int 0xff 0x87 0xd7)
    , ("orchid1"             , Vty.linearColor @Int 0xff 0x87 0xff)
    , ("orange1"             , Vty.linearColor @Int 0xff 0xaf 0x00)
    , ("sandy_brown"         , Vty.linearColor @Int 0xff 0xaf 0x5f)
    , ("light_salmon1"       , Vty.linearColor @Int 0xff 0xaf 0x87)
    , ("light_pink1"         , Vty.linearColor @Int 0xff 0xaf 0xaf)
    , ("pink1"               , Vty.linearColor @Int 0xff 0xaf 0xd7)
    , ("plum1"               , Vty.linearColor @Int 0xff 0xaf 0xff)
    , ("gold1"               , Vty.linearColor @Int 0xff 0xd7 0x00)
    , ("light_goldenrod2"    , Vty.linearColor @Int 0xff 0xd7 0x87)
    , ("navajo_white1"       , Vty.linearColor @Int 0xff 0xd7 0xaf)
    , ("misty_rose1"         , Vty.linearColor @Int 0xff 0xd7 0xd7)
    , ("thistle1"            , Vty.linearColor @Int 0xff 0xd7 0xff)
    , ("yellow1"             , Vty.linearColor @Int 0xff 0xff 0x00)
    , ("light_goldenrod1"    , Vty.linearColor @Int 0xff 0xff 0x5f)
    , ("khaki1"              , Vty.linearColor @Int 0xff 0xff 0x87)
    , ("wheat1"              , Vty.linearColor @Int 0xff 0xff 0xaf)
    , ("cornsilk1"           , Vty.linearColor @Int 0xff 0xff 0xd7)
    , ("grey100"             , Vty.linearColor @Int 0xff 0xff 0xff)
    , ("grey3"               , Vty.linearColor @Int 0x08 0x08 0x08)
    , ("grey7"               , Vty.linearColor @Int 0x12 0x12 0x12)
    , ("grey11"              , Vty.linearColor @Int 0x1c 0x1c 0x1c)
    , ("grey15"              , Vty.linearColor @Int 0x26 0x26 0x26)
    , ("grey19"              , Vty.linearColor @Int 0x30 0x30 0x30)
    , ("grey23"              , Vty.linearColor @Int 0x3a 0x3a 0x3a)
    , ("grey27"              , Vty.linearColor @Int 0x44 0x44 0x44)
    , ("grey30"              , Vty.linearColor @Int 0x4e 0x4e 0x4e)
    , ("grey35"              , Vty.linearColor @Int 0x58 0x58 0x58)
    , ("grey39"              , Vty.linearColor @Int 0x62 0x62 0x62)
    , ("grey42"              , Vty.linearColor @Int 0x6c 0x6c 0x6c)
    , ("grey46"              , Vty.linearColor @Int 0x76 0x76 0x76)
    , ("grey50"              , Vty.linearColor @Int 0x80 0x80 0x80)
    , ("grey54"              , Vty.linearColor @Int 0x8a 0x8a 0x8a)
    , ("grey58"              , Vty.linearColor @Int 0x94 0x94 0x94)
    , ("grey62"              , Vty.linearColor @Int 0x9e 0x9e 0x9e)
    , ("grey66"              , Vty.linearColor @Int 0xa8 0xa8 0xa8)
    , ("grey70"              , Vty.linearColor @Int 0xb2 0xb2 0xb2)
    , ("grey74"              , Vty.linearColor @Int 0xbc 0xbc 0xbc)
    , ("grey78"              , Vty.linearColor @Int 0xc6 0xc6 0xc6)
    , ("grey82"              , Vty.linearColor @Int 0xd0 0xd0 0xd0)
    , ("grey85"              , Vty.linearColor @Int 0xda 0xda 0xda)
    , ("grey89"              , Vty.linearColor @Int 0xe4 0xe4 0xe4)
    , ("grey93"              , Vty.linearColor @Int 0xee 0xee 0xee)
    ]
