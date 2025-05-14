{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Logging where

import Verset
import Data.Time qualified as DT


data LogLevel
  = LlDebug
  | LlInfo
  | LlWarn
  | LlError
  | LlCritical
  deriving stock (Show, Eq, Ord, Bounded, Enum)

data Logger = Logger
  { lgWarn :: !(Text -> IO ())
  , lgError :: !(Text -> IO ())
  , lgInfo :: !(Text -> IO ())
  , lgDebug :: !(Text -> IO ())
  , lgCritical :: !(Text -> IO ())
  , lgReadLast :: !(Int -> IO [LogEntry])
  }


data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leTime :: !DT.UTCTime
  , leText :: !Text
  } deriving stock (Show, Eq)



logLevelFilters :: [[LogLevel]]
logLevelFilters =
  [ [LlError]
  , [LlWarn, LlError]
  , [LlInfo, LlWarn, LlError]
  , [LlDebug, LlInfo, LlWarn, LlError]
  ]

logLevelName :: LogLevel -> Text
logLevelName LlDebug = "DEBUG"
logLevelName LlInfo = "INFO"
logLevelName LlWarn = "WARN"
logLevelName LlError = "ERROR"
logLevelName LlCritical = "CRITICAL"
