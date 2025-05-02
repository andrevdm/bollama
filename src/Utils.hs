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



attrMapFromFile :: FilePath -> IO ([Text], BA.AttrMap)
attrMapFromFile file = do
  ls1 <- Txt.lines <$> Txt.readFile file
  let ls2 = Txt.strip <$> ls1
      ls3 = filter (not . Txt.isInfixOf "--") ls2
      ls4 = filter (not . Txt.isInfixOf "//") ls3
      ls = filter (not . Txt.null) ls4
      lcs = Txt.splitOn "," <$> ls
      lines = parseLine <$> lcs
      errors = lefts lines
      attrs = rights lines
      def = snd <$> find (\(n, _) -> n == B.attrName "default") attrs
      attrMap = BA.attrMap (fromMaybe Vty.defAttr def) attrs
  pure (errors, attrMap)

  where
    parseLine :: [Text] -> Either Text (BA.AttrName, Vty.Attr)
    parseLine [n, f, b] = parseLine [n, f, b, "-"]
    parseLine [n1, f1, b1, s1] = do
      f2 <- readColour . Txt.strip $ f1
      b2 <- readColour . Txt.strip $ b1
      s2 <- readStyle . Txt.strip $ s1
      let n2 = readAttrName . Txt.strip $ n1
          s3 = fromMaybe Vty.defaultStyleMask s2

      case (f2, b2) of
        (Just f, Just b) -> pure (n2, Vty.withStyle (f `B.on` b) s3)
        (Just f, Nothing) -> pure (n2, Vty.withStyle (B.fg f) s3)
        (Nothing, Just b) -> pure (n2, Vty.withStyle (B.bg b) s3)
        (Nothing, Nothing) -> pure (n2, Vty.withStyle Vty.defAttr s3)

    parseLine vs = Left $ "Invalid line: " <> Txt.intercalate "," vs


    readColour :: Text -> Either Text (Maybe Vty.Color)
    readColour "-" = Right Nothing
    readColour "black" = Right $ Just Vty.black
    readColour "red" = Right $ Just Vty.red
    readColour "green" = Right $ Just Vty.green
    readColour "yellow" = Right $ Just Vty.yellow
    readColour "blue" = Right $ Just Vty.blue
    readColour "magenta" = Right $ Just Vty.magenta
    readColour "cyan" = Right $ Just Vty.cyan
    readColour "white" = Right $ Just Vty.white
    readColour "brightBlack" = Right $ Just Vty.brightBlack
    readColour "brightRed" = Right $ Just Vty.brightRed
    readColour "brightGreen" = Right $ Just Vty.brightGreen
    readColour "brightYellow" = Right $ Just Vty.brightYellow
    readColour "brightBlue" = Right $ Just Vty.brightBlue
    readColour "brightMagenta" = Right $ Just Vty.brightMagenta
    readColour "brightCyan" = Right $ Just Vty.brightCyan
    readColour "brightWhite" = Right $ Just Vty.brightWhite
    readColour "orange" = Right $ Just $ Vty.rgbColor @Int 0xfa 0xa5 0x00
    readColour "grey" = Right $ Just $ Vty.rgbColor @Int 128 128 128
    readColour "greyA" = Right $ Just $ Vty.rgbColor @Int 100 100 80
    readColour "greyB" = Right $ Just $ Vty.rgbColor @Int 90 90 0
    readColour "skyBlue" = Right $ Just $ Vty.rgbColor @Int 0x87 0xce 0xeb
    readColour "slateBlue" = Right $ Just $ Vty.rgbColor @Int 0x6a 0x5a 0xcd
    readColour "pink" = Right $ Just $ Vty.rgbColor @Int 0xff 0xc0 0xcb
    readColour t = do
      let clr = do
           _ <- if Txt.isPrefixOf "#" t then pure () else Nothing
           a <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 1 $ t)
           b <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 3 $ t)
           c <- readMaybe $ "0x" <> (Txt.unpack . Txt.take 2 . Txt.drop 5 $ t)
           pure $ Vty.rgbColor @Int a b c
      case clr of
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
