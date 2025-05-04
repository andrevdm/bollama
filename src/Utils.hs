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
import Brick.Widgets.Dialog qualified as BD
import Brick.Widgets.ProgressBar qualified as BP
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Txt
import Data.Text qualified as Txt
import Data.UUID.V4 qualified as UU
import Graphics.Vty qualified as Vty
import Ollama qualified as O
import Text.Printf (printf)


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


attrMapFromFile :: FilePath -> IO ([Text], BA.AttrMap)
attrMapFromFile file = do
  txt <- Txt.readFile file
  attrMapFromText txt


attrMapFromText :: Text -> IO ([Text], BA.AttrMap)
attrMapFromText txt = do
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

      -- Add all known colours as _fg_ and _bg_ attributes
      knownAsFs = knownColours <&> \ (n, c) -> (B.attrName . Txt.unpack $ "_fg_" <> n, B.fg c)
      knownAsBs = knownColours <&> \ (n, c) -> (B.attrName . Txt.unpack $ "_bg_" <> n, B.bg c)

      -- Combine with user defined attributes, prefer user defined
      attrs = Map.toList $ Map.union (Map.fromList attrs1) (Map.fromList $ knownAsBs <> knownAsFs)

      -- Set the default attribute if it exists
      def = snd <$> find (\(n, _) -> n == B.attrName "default") attrs
      attrMap = BA.attrMap (fromMaybe Vty.defAttr def) attrs
  pure (errors, attrMap)

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
                pure $ Vty.rgbColor @Int a b c
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
    readAttrName t = BA.attrName . Txt.unpack $ t



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
    , ("orange",              Vty.rgbColor @Int 0xFA 0xA5 0x00)
    , ("grey",                Vty.rgbColor @Int 128  128 128)
    , ("greyA",               Vty.rgbColor @Int 100  100  80)
    , ("greyB",               Vty.rgbColor @Int  90   90   0)
    , ("skyBlue",             Vty.rgbColor @Int 0x87 0xCE 0xEB)
    , ("slateBlue",           Vty.rgbColor @Int 0x6A 0x5A 0xCD)
    , ("pink",                Vty.rgbColor @Int 0xFF 0xC0 0xCB)

    , ("aquamarine",          Vty.rgbColor @Int 0x5F 0xFF 0xD7)
    , ("aquamarine1",         Vty.rgbColor @Int 0x87 0xFF 0xD7)
    , ("aquamarine3",         Vty.rgbColor @Int 0x5F 0xD7 0xAF)
    , ("blue1",               Vty.rgbColor @Int 0x00 0x00 0xFF)
    , ("blue3",               Vty.rgbColor @Int 0x00 0x00 0xAF)
    , ("blue4",               Vty.rgbColor @Int 0x00 0x00 0xD7)
    , ("blueViolet",          Vty.rgbColor @Int 0x5F 0x00 0xFF)
    , ("cadetBlue",           Vty.rgbColor @Int 0x5F 0xAF 0x87)
    , ("cadetBlue2",          Vty.rgbColor @Int 0x5F 0xAF 0xAF)
    , ("chartreuse1",         Vty.rgbColor @Int 0x87 0xFF 0x00)
    , ("chartreuse2",         Vty.rgbColor @Int 0x5F 0xFF 0x00)
    , ("chartreuse6",         Vty.rgbColor @Int 0x87 0xD7 0x00)
    , ("chartreuse3",         Vty.rgbColor @Int 0x5F 0xAF 0x00)
    , ("chartreuse7",         Vty.rgbColor @Int 0x5F 0xD7 0x00)
    , ("chartreuse4",         Vty.rgbColor @Int 0x5F 0x87 0x00)
    , ("cornflowerBlue",      Vty.rgbColor @Int 0x5F 0x87 0xFF)
    , ("cornsilk1",           Vty.rgbColor @Int 0xFF 0xFF 0xD7)
    , ("cyan1",               Vty.rgbColor @Int 0x00 0xFF 0xFF)
    , ("cyan2",               Vty.rgbColor @Int 0x00 0xFF 0xD7)
    , ("cyan3",               Vty.rgbColor @Int 0x00 0xD7 0xAF)
    , ("darkBlue",            Vty.rgbColor @Int 0x00 0x00 0x87)
    , ("darkCyan",            Vty.rgbColor @Int 0x00 0xAF 0x87)
    , ("darkGoldenrod",       Vty.rgbColor @Int 0xAF 0x87 0x00)
    , ("darkGreen",           Vty.rgbColor @Int 0x00 0x5F 0x00)
    , ("darkKhaki",           Vty.rgbColor @Int 0xAF 0xAF 0x5F)
    , ("darkMagenta",         Vty.rgbColor @Int 0x87 0x00 0x87)
    , ("darkMagenta2",        Vty.rgbColor @Int 0x87 0x00 0xAF)
    , ("darkOliveGreen1",     Vty.rgbColor @Int 0xD7 0xFF 0x5F)
    , ("darkOliveGreen4",     Vty.rgbColor @Int 0xD7 0xFF 0x87)
    , ("darkOliveGreen2",     Vty.rgbColor @Int 0xAF 0xFF 0x5F)
    , ("darkOliveGreen3",     Vty.rgbColor @Int 0x87 0xAF 0x5F)
    , ("darkOliveGreen6",     Vty.rgbColor @Int 0x87 0xD7 0x5F)
    , ("darkOliveGreen5",     Vty.rgbColor @Int 0xAF 0xD7 0x5F)
    , ("darkOrange3",         Vty.rgbColor @Int 0xAF 0x5F 0x00)
    , ("darkOrange4",         Vty.rgbColor @Int 0xD7 0x5F 0x00)
    , ("darkOrange",          Vty.rgbColor @Int 0xFF 0x87 0x00)
    , ("darkRed",             Vty.rgbColor @Int 0x5F 0x00 0x00)
    , ("darkRed2",            Vty.rgbColor @Int 0x87 0x00 0x00)
    , ("darkSeaGreen1",       Vty.rgbColor @Int 0xAF 0xFF 0xD7)
    , ("darkSeaGreen8",       Vty.rgbColor @Int 0xD7 0xFF 0xAF)
    , ("darkSeaGreen2",       Vty.rgbColor @Int 0xAF 0xD7 0xAF)
    , ("darkSeaGreen3",       Vty.rgbColor @Int 0xAF 0xFF 0xAF)
    , ("darkSeaGreen4",       Vty.rgbColor @Int 0x87 0xD7 0xAF)
    , ("darkSeaGreen5",       Vty.rgbColor @Int 0xAF 0xD7 0x87)
    , ("darkSeaGreen6",       Vty.rgbColor @Int 0x5F 0x87 0x5F)
    , ("darkSeaGreen7",       Vty.rgbColor @Int 0x5F 0xAF 0x5F)
    , ("darkSeaGreen",        Vty.rgbColor @Int 0x87 0xAF 0x87)
    , ("darkSlateGray1",      Vty.rgbColor @Int 0x87 0xFF 0xFF)
    , ("darkSlateGray2",      Vty.rgbColor @Int 0x5F 0xFF 0xFF)
    , ("darkSlateGray3",      Vty.rgbColor @Int 0x87 0xD7 0xD7)
    , ("darkTurquoise",       Vty.rgbColor @Int 0x00 0xD7 0xD7)
    , ("darkViolet",          Vty.rgbColor @Int 0x87 0x00 0xD7)
    , ("darkViolet2",         Vty.rgbColor @Int 0xAF 0x00 0xD7)
    , ("deepPink1",           Vty.rgbColor @Int 0xFF 0x00 0x87)
    , ("deepPink7",           Vty.rgbColor @Int 0xFF 0x00 0xAF)
    , ("deepPink2",           Vty.rgbColor @Int 0xFF 0x00 0x5F)
    , ("deepPink3",           Vty.rgbColor @Int 0xD7 0x00 0x5F)
    , ("deepPink5",           Vty.rgbColor @Int 0xD7 0x00 0x87)
    , ("deepPink4",           Vty.rgbColor @Int 0x5F 0x00 0x5F)
    , ("deepPink6",           Vty.rgbColor @Int 0x87 0x00 0x5F)
    , ("deepPink8",           Vty.rgbColor @Int 0xAF 0x00 0x5F)
    , ("deepSkyBlue1",        Vty.rgbColor @Int 0x00 0xAF 0xFF)
    , ("deepSkyBlue2",        Vty.rgbColor @Int 0x00 0xAF 0xD7)
    , ("deepSkyBlue3",        Vty.rgbColor @Int 0x00 0x87 0xAF)
    , ("deepSkyBlue4",        Vty.rgbColor @Int 0x00 0x87 0xD7)
    , ("deepSkyBlue5",        Vty.rgbColor @Int 0x00 0x5F 0x5F)
    , ("deepSkyBlue6",        Vty.rgbColor @Int 0x00 0x5F 0x87)
    , ("deepSkyBlue7",        Vty.rgbColor @Int 0x00 0x5F 0xAF)
    , ("dodgerBlue1",         Vty.rgbColor @Int 0x00 0x87 0xFF)
    , ("dodgerBlue2",         Vty.rgbColor @Int 0x00 0x5F 0xFF)
    , ("dodgerBlue3",         Vty.rgbColor @Int 0x00 0x5F 0xD7)
    , ("gold1",               Vty.rgbColor @Int 0xFF 0xD7 0x00)
    , ("gold3",               Vty.rgbColor @Int 0xAF 0xAF 0x00)
    , ("gold4",               Vty.rgbColor @Int 0xD7 0xAF 0x00)
    , ("green1",              Vty.rgbColor @Int 0x00 0xFF 0x00)
    , ("green3",              Vty.rgbColor @Int 0x00 0xAF 0x00)
    , ("green4",              Vty.rgbColor @Int 0x00 0xD7 0x00)
    , ("green5",              Vty.rgbColor @Int 0x00 0x87 0x00)
    , ("greenYellow",         Vty.rgbColor @Int 0xAF 0xFF 0x00)
    , ("grey0",               Vty.rgbColor @Int 0x00 0x00 0x00)
    , ("grey100",             Vty.rgbColor @Int 0xFF 0xFF 0xFF)
    , ("grey11",              Vty.rgbColor @Int 0x1C 0x1C 0x1C)
    , ("grey15",              Vty.rgbColor @Int 0x26 0x26 0x26)
    , ("grey19",              Vty.rgbColor @Int 0x30 0x30 0x30)
    , ("grey23",              Vty.rgbColor @Int 0x3A 0x3A 0x3A)
    , ("grey27",              Vty.rgbColor @Int 0x44 0x44 0x44)
    , ("grey30",              Vty.rgbColor @Int 0x4E 0x4E 0x4E)
    , ("grey35",              Vty.rgbColor @Int 0x58 0x58 0x58)
    , ("grey37",              Vty.rgbColor @Int 0x5F 0x5F 0x5F)
    , ("grey39",              Vty.rgbColor @Int 0x62 0x62 0x62)
    , ("grey3",               Vty.rgbColor @Int 0x08 0x08 0x08)
    , ("grey42",              Vty.rgbColor @Int 0x6C 0x6C 0x6C)
    , ("grey46",              Vty.rgbColor @Int 0x76 0x76 0x76)
    , ("grey50",              Vty.rgbColor @Int 0x80 0x80 0x80)
    , ("grey53",              Vty.rgbColor @Int 0x87 0x87 0x87)
    , ("grey54",              Vty.rgbColor @Int 0x8A 0x8A 0x8A)
    , ("grey58",              Vty.rgbColor @Int 0x94 0x94 0x94)
    , ("grey62",              Vty.rgbColor @Int 0x9E 0x9E 0x9E)
    , ("grey63",              Vty.rgbColor @Int 0xAF 0x87 0xAF)
    , ("grey66",              Vty.rgbColor @Int 0xA8 0xA8 0xA8)
    , ("grey69",              Vty.rgbColor @Int 0xAF 0xAF 0xAF)
    , ("grey70",              Vty.rgbColor @Int 0xB2 0xB2 0xB2)
    , ("grey74",              Vty.rgbColor @Int 0xBC 0xBC 0xBC)
    , ("grey78",              Vty.rgbColor @Int 0xC6 0xC6 0xC6)
    , ("grey7",               Vty.rgbColor @Int 0x12 0x12 0x12)
    , ("grey82",              Vty.rgbColor @Int 0xD0 0xD0 0xD0)
    , ("grey84",              Vty.rgbColor @Int 0xD7 0xD7 0xD7)
    , ("grey85",              Vty.rgbColor @Int 0xDA 0xDA 0xDA)
    , ("grey89",              Vty.rgbColor @Int 0xE4 0xE4 0xE4)
    , ("grey93",              Vty.rgbColor @Int 0xEE 0xEE 0xEE)
    , ("honeydew2",           Vty.rgbColor @Int 0xD7 0xFF 0xD7)
    , ("hotPink2",            Vty.rgbColor @Int 0xD7 0x5F 0xAF)
    , ("hotPink3",            Vty.rgbColor @Int 0xAF 0x5F 0x87)
    , ("hotPink4",            Vty.rgbColor @Int 0xD7 0x5F 0x87)
    , ("hotPink",             Vty.rgbColor @Int 0xFF 0x5F 0xAF)
    , ("hotPink5",            Vty.rgbColor @Int 0xFF 0x5F 0xD7)
    , ("indianRed1",          Vty.rgbColor @Int 0xFF 0x5F 0x5F)
    , ("indianRed2",          Vty.rgbColor @Int 0xFF 0x5F 0x87)
    , ("indianRed",           Vty.rgbColor @Int 0xAF 0x5F 0x5F)
    , ("indianRed3",          Vty.rgbColor @Int 0xD7 0x5F 0x5F)
    , ("khaki1",              Vty.rgbColor @Int 0xFF 0xFF 0x87)
    , ("khaki3",              Vty.rgbColor @Int 0xD7 0xD7 0x5F)
    , ("lightCoral",          Vty.rgbColor @Int 0xFF 0x87 0x87)
    , ("lightCyan1",          Vty.rgbColor @Int 0xD7 0xFF 0xFF)
    , ("lightCyan3",          Vty.rgbColor @Int 0xAF 0xD7 0xD7)
    , ("lightGoldenrod1",     Vty.rgbColor @Int 0xFF 0xFF 0x5F)
    , ("lightGoldenrod2",     Vty.rgbColor @Int 0xD7 0xD7 0x87)
    , ("lightGoldenrod4",     Vty.rgbColor @Int 0xFF 0xD7 0x5F)
    , ("lightGoldenrod5",     Vty.rgbColor @Int 0xFF 0xD7 0x87)
    , ("lightGoldenrod3",     Vty.rgbColor @Int 0xD7 0xAF 0x5F)
    , ("lightGreen",          Vty.rgbColor @Int 0x87 0xFF 0x5F)
    , ("lightGreen2",         Vty.rgbColor @Int 0x87 0xFF 0x87)
    , ("lightPink1",          Vty.rgbColor @Int 0xFF 0xAF 0xAF)
    , ("lightPink3",          Vty.rgbColor @Int 0xD7 0x87 0x87)
    , ("lightPink4",          Vty.rgbColor @Int 0x87 0x5F 0x5F)
    , ("lightSalmon1",        Vty.rgbColor @Int 0xFF 0xAF 0x87)
    , ("lightSalmon3",        Vty.rgbColor @Int 0xAF 0x87 0x5F)
    , ("lightSalmon4",        Vty.rgbColor @Int 0xD7 0x87 0x5F)
    , ("lightSeaGreen",       Vty.rgbColor @Int 0x00 0xAF 0xAF)
    , ("lightSkyBlue1",       Vty.rgbColor @Int 0xAF 0xD7 0xFF)
    , ("lightSkyBlue3",       Vty.rgbColor @Int 0x87 0xAF 0xAF)
    , ("lightSkyBlue4",       Vty.rgbColor @Int 0x87 0xAF 0xD7)
    , ("lightSlateBlue",      Vty.rgbColor @Int 0x87 0x87 0xFF)
    , ("lightSlateGrey",      Vty.rgbColor @Int 0x87 0x87 0xAF)
    , ("lightSteelBlue1",     Vty.rgbColor @Int 0xD7 0xD7 0xFF)
    , ("lightSteelBlue3",     Vty.rgbColor @Int 0xAF 0xAF 0xD7)
    , ("lightSteelBlue",      Vty.rgbColor @Int 0xAF 0xAF 0xFF)
    , ("lightYellow3",        Vty.rgbColor @Int 0xD7 0xD7 0xAF)
    , ("magenta1",            Vty.rgbColor @Int 0xFF 0x00 0xFF)
    , ("magenta2",            Vty.rgbColor @Int 0xD7 0x00 0xFF)
    , ("magenta5",            Vty.rgbColor @Int 0xFF 0x00 0xD7)
    , ("magenta3",            Vty.rgbColor @Int 0xAF 0x00 0xAF)
    , ("magenta4",            Vty.rgbColor @Int 0xD7 0x00 0xAF)
    , ("magenta6",            Vty.rgbColor @Int 0xD7 0x00 0xD7)
    , ("mediumOrchid1",       Vty.rgbColor @Int 0xD7 0x5F 0xFF)
    , ("mediumOrchid2",       Vty.rgbColor @Int 0xFF 0x5F 0xFF)
    , ("mediumOrchid3",       Vty.rgbColor @Int 0xAF 0x5F 0xAF)
    , ("mediumOrchid",        Vty.rgbColor @Int 0xAF 0x5F 0xD7)
    , ("mediumPurple1",       Vty.rgbColor @Int 0xAF 0x87 0xFF)
    , ("mediumPurple2",       Vty.rgbColor @Int 0xAF 0x5F 0xFF)
    , ("mediumPurple5",       Vty.rgbColor @Int 0xAF 0x87 0xD7)
    , ("mediumPurple3",       Vty.rgbColor @Int 0x87 0x5F 0xAF)
    , ("mediumPurple6",       Vty.rgbColor @Int 0x87 0x5F 0xD7)
    , ("mediumPurple4",       Vty.rgbColor @Int 0x5F 0x5F 0x87)
    , ("mediumPurple",        Vty.rgbColor @Int 0x87 0x87 0xD7)
    , ("mediumSpringGreen",   Vty.rgbColor @Int 0x00 0xFF 0xAF)
    , ("mediumTurquoise",     Vty.rgbColor @Int 0x5F 0xD7 0xD7)
    , ("mediumVioletRed",     Vty.rgbColor @Int 0xAF 0x00 0x87)
    , ("mistyRose1",          Vty.rgbColor @Int 0xFF 0xD7 0xD7)
    , ("mistyRose3",          Vty.rgbColor @Int 0xD7 0xAF 0xAF)
    , ("navajoWhite1",        Vty.rgbColor @Int 0xFF 0xD7 0xAF)
    , ("navajoWhite3",        Vty.rgbColor @Int 0xAF 0xAF 0x87)
    , ("navyBlue",            Vty.rgbColor @Int 0x00 0x00 0x5F)
    , ("orange1",             Vty.rgbColor @Int 0xFF 0xAF 0x00)
    , ("orange3",             Vty.rgbColor @Int 0xD7 0x87 0x00)
    , ("orange4",             Vty.rgbColor @Int 0x5F 0x5F 0x00)
    , ("orange5",             Vty.rgbColor @Int 0x87 0x5F 0x00)
    , ("orangeRed1",          Vty.rgbColor @Int 0xFF 0x5F 0x00)
    , ("orchid1",             Vty.rgbColor @Int 0xFF 0x87 0xFF)
    , ("orchid2",             Vty.rgbColor @Int 0xFF 0x87 0xD7)
    , ("orchid",              Vty.rgbColor @Int 0xD7 0x5F 0xD7)
    , ("paleGreen1",          Vty.rgbColor @Int 0x87 0xFF 0xAF)
    , ("paleGreen2",          Vty.rgbColor @Int 0xAF 0xFF 0x87)
    , ("paleGreen3",          Vty.rgbColor @Int 0x5F 0xD7 0x5F)
    , ("paleGreen4",          Vty.rgbColor @Int 0x87 0xD7 0x87)
    , ("paleTurquoise1",      Vty.rgbColor @Int 0xAF 0xFF 0xFF)
    , ("paleTurquoise4",      Vty.rgbColor @Int 0x5F 0x87 0x87)
    , ("paleVioletRed1",      Vty.rgbColor @Int 0xFF 0x87 0xAF)
    , ("pink1",               Vty.rgbColor @Int 0xFF 0xAF 0xD7)
    , ("pink3",               Vty.rgbColor @Int 0xD7 0x87 0xAF)
    , ("plum1",               Vty.rgbColor @Int 0xFF 0xAF 0xFF)
    , ("plum2",               Vty.rgbColor @Int 0xD7 0xAF 0xFF)
    , ("plum3",               Vty.rgbColor @Int 0xD7 0x87 0xD7)
    , ("plum4",               Vty.rgbColor @Int 0x87 0x5F 0x87)
    , ("purple3",             Vty.rgbColor @Int 0x5F 0x00 0xD7)
    , ("purple4",             Vty.rgbColor @Int 0x5F 0x00 0x87)
    , ("purple5",             Vty.rgbColor @Int 0x5F 0x00 0xAF)
    , ("purple",              Vty.rgbColor @Int 0x87 0x00 0xFF)
    , ("purple6",             Vty.rgbColor @Int 0xAF 0x00 0xFF)
    , ("red1",                Vty.rgbColor @Int 0xFF 0x00 0x00)
    , ("red3",                Vty.rgbColor @Int 0xAF 0x00 0x00)
    , ("red4",                Vty.rgbColor @Int 0xD7 0x00 0x00)
    , ("rosyBrown",           Vty.rgbColor @Int 0xAF 0x87 0x87)
    , ("royalBlue1",          Vty.rgbColor @Int 0x5F 0x5F 0xFF)
    , ("salmon1",             Vty.rgbColor @Int 0xFF 0x87 0x5F)
    , ("sandyBrown",          Vty.rgbColor @Int 0xFF 0xAF 0x5F)
    , ("seaGreen1",           Vty.rgbColor @Int 0x5F 0xFF 0x87)
    , ("seaGreen4",           Vty.rgbColor @Int 0x5F 0xFF 0xAF)
    , ("seaGreen2",           Vty.rgbColor @Int 0x5F 0xFF 0x5F)
    , ("seaGreen3",           Vty.rgbColor @Int 0x5F 0xD7 0x87)
    , ("skyBlue1",            Vty.rgbColor @Int 0x87 0xD7 0xFF)
    , ("skyBlue2",            Vty.rgbColor @Int 0x87 0xAF 0xFF)
    , ("skyBlue3",            Vty.rgbColor @Int 0x5F 0xAF 0xD7)
    , ("slateBlue1",          Vty.rgbColor @Int 0x87 0x5F 0xFF)
    , ("slateBlue3",          Vty.rgbColor @Int 0x5F 0x5F 0xAF)
    , ("slateBlue4",          Vty.rgbColor @Int 0x5F 0x5F 0xD7)
    , ("springGreen1",        Vty.rgbColor @Int 0x00 0xFF 0x87)
    , ("springGreen2",        Vty.rgbColor @Int 0x00 0xD7 0x87)
    , ("springGreen5",        Vty.rgbColor @Int 0x00 0xFF 0x5F)
    , ("springGreen3",        Vty.rgbColor @Int 0x00 0xAF 0x5F)
    , ("springGreen6",        Vty.rgbColor @Int 0x00 0xD7 0x5F)
    , ("springGreen4",        Vty.rgbColor @Int 0x00 0x87 0x5F)
    , ("steelBlue1",          Vty.rgbColor @Int 0x5F 0xAF 0xFF)
    , ("steelBlue2",          Vty.rgbColor @Int 0x5F 0xD7 0xFF)
    , ("steelBlue3",          Vty.rgbColor @Int 0x5F 0x87 0xD7)
    , ("steelBlue",           Vty.rgbColor @Int 0x5F 0x87 0xAF)
    , ("tan",                 Vty.rgbColor @Int 0xD7 0xAF 0x87)
    , ("thistle1",            Vty.rgbColor @Int 0xFF 0xD7 0xFF)
    , ("thistle3",            Vty.rgbColor @Int 0xD7 0xAF 0xD7)
    , ("turquoise2",          Vty.rgbColor @Int 0x00 0xD7 0xFF)
    , ("turquoise4",          Vty.rgbColor @Int 0x00 0x87 0x87)
    , ("violet",              Vty.rgbColor @Int 0xD7 0x87 0xFF)
    , ("wheat1",              Vty.rgbColor @Int 0xFF 0xFF 0xAF)
    , ("wheat4",              Vty.rgbColor @Int 0x87 0x87 0x5F)
    , ("yellow1",             Vty.rgbColor @Int 0xFF 0xFF 0x00)
    , ("yellow2",             Vty.rgbColor @Int 0xD7 0xFF 0x00)
    , ("yellow3",             Vty.rgbColor @Int 0xAF 0xD7 0x00)
    , ("yellow5",             Vty.rgbColor @Int 0xD7 0xD7 0x00)
    , ("yellow4",             Vty.rgbColor @Int 0x87 0x87 0x00)
    , ("yellow6",             Vty.rgbColor @Int 0x87 0xAF 0x00)
    ]
