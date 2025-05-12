{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Config where

import           Verset

import Brick.AttrMap qualified as BA
import Data.Aeson qualified as Ae
import Data.Aeson.Encode.Pretty qualified as Ae
import Data.ByteString.Lazy as BSL
import Data.Text qualified as Txt
import Data.Version qualified as Ver
import Paths_bollama qualified as Paths
import System.Directory qualified as Dir
import System.FilePath ((</>))

import Core qualified as C
import Utils qualified as U


verText :: Text
verText = Txt.pack $ Ver.showVersion Paths.version <> " beta"


getConfigDir :: IO FilePath
getConfigDir = do
  p <- Dir.getXdgDirectory Dir.XdgConfig "bollama"
  Dir.createDirectoryIfMissing True p
  pure p


getStateDir :: IO FilePath
getStateDir = do
  p <- Dir.getXdgDirectory Dir.XdgState "bollama"
  Dir.createDirectoryIfMissing True p
  pure p


getDataDir :: IO FilePath
getDataDir = do
  p <- Dir.getXdgDirectory Dir.XdgData "bollama"
  Dir.createDirectoryIfMissing True p
  pure p


emptyAppConfig :: C.AppConfig
emptyAppConfig = C.AppConfig
  { acModelTag = mempty
  , acDefaultModel = Just mempty
  , acDefaultChat = Just mempty
  , acAvoidEmojis = False
  , acDefaultTab = C.TabModels
  , acOllamaUrl = Nothing
  , acAllowMouse = True
  }


loadAppConfig :: IO C.AppConfig
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
          pure emptyAppConfig


writeAppConfig :: C.AppConfig -> IO ()
writeAppConfig cfg = do
  configDir <- getConfigDir
  Dir.createDirectoryIfMissing True configDir

  let configFile = configDir </> "config.json"
  let bs = Ae.encodePretty cfg
  BSL.writeFile configFile bs


defaultModel :: C.AppConfig -> Text
defaultModel cfg = fromMaybe "qwen3:0.6b" $ cfg.acDefaultModel
  where


loadTheme :: IO ([Text], BA.AttrMap)
loadTheme = do
  configDir <- getConfigDir
  let themeFile = configDir </> "theme.csv"

  Dir.doesFileExist themeFile >>= \case
    True -> U.attrMapFromFile themeFile
    False -> U.attrMapFromText U.defaultTheme


