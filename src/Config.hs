{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Config where

import           Verset

import Data.Aeson qualified as Ae
import Data.Text qualified as Txt
import Data.Version qualified as Ver
import Paths_bollama qualified as Paths
import System.Directory qualified as Dir
import System.FilePath ((</>))

import Core qualified as C



verText :: Text
verText = Txt.pack $ Ver.showVersion Paths.version


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


emptyAppConfig :: C.AppConfig
emptyAppConfig = C.AppConfig
  { acModelTag = mempty
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
          pure $ C.AppConfig mempty


writeAppConfig :: C.AppConfig -> IO ()
writeAppConfig cfg = do
  configDir <- getConfigDir
  Dir.createDirectoryIfMissing True configDir

  let configFile = configDir </> "config.json"
  Ae.encodeFile configFile cfg
