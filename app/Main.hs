{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Verset

import qualified App


main :: IO ()
main = App.runTui
