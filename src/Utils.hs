{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Utils where

import           Verset

import Data.Text qualified as Txt
import Ollama qualified as O
import Text.Printf (printf)

import Core qualified as C



parseParams :: C.ModelItem -> Maybe Double
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
