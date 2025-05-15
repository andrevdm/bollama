{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

module Utils where

import Verset
import Brick qualified as B
import Control.Lens ((.=), use)
import Data.Text qualified as Txt
import Data.Time qualified as Dt
import Data.UUID.V4 qualified as UU
import Ollama qualified as O
import Text.Printf (printf)

import Core qualified as C


parseParams :: O.ModelDetails -> Maybe Double
parseParams md =
  let sz = md.parameterSize
      (mul, drop') =
        if | Txt.isSuffixOf "B" sz -> (1_000_000_000.0, 1)
           | Txt.isSuffixOf "M" sz -> (1_000_000.0, 1)
           | otherwise -> (1, 0)

      sz2 = Txt.dropEnd drop' sz
  in
  case readMaybe @Double . Txt.unpack . Txt.replace "," "" $ sz2 of
    Nothing -> Nothing
    Just sz3 -> Just $ sz3 * mul


formatParamSize :: Maybe Double -> Text
formatParamSize Nothing = ""
formatParamSize (Just d) = (Txt.pack (printf "%.2f" (d / 1_000_000_000.0))) <> "B"


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



newUuidText :: IO Text
newUuidText = do
  uuid <- UU.nextRandom
  pure $ show uuid


removeThink :: Text -> Text
removeThink str =
  if not (Txt.isPrefixOf "<think>" str)
  then str
  else
    let ls = drop 1 . Txt.lines $ str in
    case findIndex (== "</think>") ls of
      Nothing -> str
      Just i -> Txt.strip . Txt.unlines . drop (i + 1) $ ls



setFooterMessage :: Int -> Text -> B.EventM C.Name C.UiState ()
setFooterMessage seconds msg = do
  now <- use C.stNow
  let until = Dt.addUTCTime (fromIntegral seconds) now
  C.stFooterMessage .= Just (until, msg)


