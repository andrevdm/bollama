{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Help where

import Verset
import Brick ((<+>))
import Brick.AttrMap qualified as BA
import Brick qualified as B
import Brick.Widgets.Border qualified as BB
import Data.Text qualified as Txt

import Config qualified as Cfg
import Core qualified as C
import Theme qualified as T


data Help
  = HlpText ![Text]
  | HlpCode ![Text]
  | HlpH1 !Text
  | HlpH2 !Text
  | HlpAnchor !Text !Help
  | HlpSectionBreak
  | HlpIndent !Int ![Help]
  | HlpSection !Int !Text ![Help]
  | HlpKeys !Text ![(Text, Text)]


renderHelp :: Maybe Text -> [Help] -> B.Widget C.Name
renderHelp makeVis hs =
  B.vBox $ renderHelpItem <$> hs

  where
    renderHelpItem (HlpText ts) = B.txtWrap $ Txt.unlines ts
    renderHelpItem (HlpCode ts) = renderHelpItem (HlpText ts)
    renderHelpItem (HlpH1 t) = B.padBottom (B.Pad 1) . B.vLimit 1 . B.withAttr (B.attrName "helpH1") $ (B.txt t <+> B.fill ' ')
    renderHelpItem (HlpH2 t) = B.padBottom (B.Pad 1) . B.vLimit 1 . B.withAttr (B.attrName "helpH2") $ (B.txt t <+> B.fill ' ')

    renderHelpItem (HlpAnchor a h) =
      if makeVis == Just a
        then {--B.visible $--} renderHelpItem h
        else renderHelpItem h

    renderHelpItem HlpSectionBreak =
      B.padTop (B.Pad 3) $ B.txt  " "

    renderHelpItem (HlpIndent i hs') =
      let
        s = replicate i ' '
      in
      B.str s <+> renderHelp makeVis hs'

    renderHelpItem (HlpSection n t hs') =
      let header = if n == 1 then HlpH1 else HlpH2 in
      renderHelp makeVis
        [ HlpAnchor (Txt.toLower t) $ header t
        , HlpIndent 3 hs'
        , HlpSectionBreak
        ]

    renderHelpItem (HlpKeys title ks) =
      B.padTop (B.Pad 1) . B.padBottom (B.Pad 1) $
      BB.borderWithLabel (B.withAttr (B.attrName "helpKeyTitle") $ B.txt title) $
      (B.padAll 1 $ B.vBox (renderKey <$> ks))


    renderKey (k, v) =
      B.hBox
        [ col 12 (k <> ": ") "helpKeyShortcut"
        , B.withAttr (B.attrName "helpKeyAction") . B.txt $ v
        ]


renderHelpAsMarkdown :: [Help] -> Text
renderHelpAsMarkdown hs =
  Txt.unlines $ renderMdHelpItem <$> hs

  where
    renderMdHelpItem (HlpText ts) = mdEncode . Txt.unlines $ ts
    renderMdHelpItem (HlpCode ts) = Txt.unlines $ ["```\n", Txt.unlines ts, "\n```"]
    renderMdHelpItem (HlpH1 t) = Txt.unlines $ [ "# " <> t ]
    renderMdHelpItem (HlpH2 t) = Txt.unlines $ [ "## " <> t ]
    renderMdHelpItem (HlpAnchor _ h) = renderMdHelpItem h
    renderMdHelpItem HlpSectionBreak = "---"
    renderMdHelpItem (HlpIndent i hs') = "<div style='margin-left: " <> (show $ i * 5) <> "px'>\n\n" <> renderHelpAsMarkdown hs' <> "\n</div>\n\n"
    renderMdHelpItem (HlpSection n t hs') =
      let header = if n == 1 then HlpH1 else HlpH2 in
      renderHelpAsMarkdown
        [ HlpAnchor (Txt.toLower t) $ header t
        , HlpIndent 3 hs'
        , HlpSectionBreak
        ]
    renderMdHelpItem (HlpKeys title ks) =
      Txt.unlines $
        [ "\n\n### " <> title
        , ""
        , "| key | action |"
        , "| --- | --- |"
        ]
        <>
        (ks <&> \(k, v) -> "| " <> k <> " | " <> v <> " |")
        <>
        ["\n\n"]

    mdEncode = Txt.replace ">" "&gt;" . Txt.replace "<" "&lt;"



helpContent :: [Help]
helpContent =
  [ HlpSection 1 "Bollama"
       [ HlpText
           [ "Bollama is a simple TUI for Ollama."
           , "  version: " <> Cfg.verText
           , ""
           ]
       , HlpKeys "Global Shortcuts"
           [ ("Ctrl-q", "quit application")
           , ("Ctrl-u", "update theme from config file")
           , ("F1", "help")
           , ("F2", "Models tab")
           , ("F3", "Running/PS tab")
           , ("F4", "Chat tab")
           , ("F11", "Supported colour names")
           , ("F12", "Debug log")
           ]
       ]

  , HlpSection 1 "Tabs"
       [  HlpSection 2 "Models"
            [ HlpText
                [ "Shows the list of models available in the Ollama server sorted by parameter size."
                , "Shows the number of parameters, model size, capabilities, etc where available."
                , "You can also add a comment to a model"
                ]
            , HlpKeys "Model Shortcuts"
                [ ("c", "Edit user comment for the model")
                , ("*", "Set the model as the default for new temp chats")
                , ("d", "Delete the model")
                , ("/", "Filter models")
                , ("F5", "Refresh the model list")
                , ("t", "Use the `#Temp` chat with the selected model")
                , ("n", "Start a new chat with the selected model")
                ]
            ]

       , HlpSection 2 "PS"
            [ HlpText
                [ "Shows the list of running models."
                ]
            , HlpKeys "PS Shortcuts"
                [ ("s", "Stop selected model.  (Sends a TTL of 0)")
                ]
            ]

       , HlpSection 2 "Chat"
            [ HlpText
                [ "Shows the list of chats."
                ]
            , HlpKeys "Chat Shortcuts - global"
                [ ("F10", "Context menu")
                , ("Ctrl-n", "New chat. Start name with `#` for a temporary chat")
                , ("Ctrl-e", "Edit chat name & model")
                , ("Ctrl-t", "Toggle show thinking")
                , ("Ctrl-p", "Toggle show message details")
                , ("Ctrl-c", "Cancel current chat, assuming LLM is streaming a response")
                , ("PgUp", "Scroll chat history up")
                , ("PgDown", "Scroll chat history down")
                , ("Ctrl-PgUp", "Scroll chat history to top")
                , ("Ctrl-PgDown", "Scroll chat history to end")
                ]
            , HlpText
                [ "Note that when editing a chat, you may not change from a temp chat to a non-temp chat or vice versa."
                ]
            , HlpKeys "Chat Shortcuts - chat input"
                [ ("Ctrl-s", "Send the current message")
                ]
            , HlpKeys "Chat Shortcuts - chats list"
                [ ("*", "Set as the default chat")
                ]
            ]

       , HlpSection 2 "Colours"
            [ HlpText
                [ "Shows the list of known colour names available for use in the TUI theme"
                , "See *Themes* below"
                ]
            ]

       , HlpSection 2 "Log"
            [ HlpText
                [ "Shows the log of events."
                ]
            ]
       ]

  , HlpSection 1 "Configuration and State"
       [ HlpText
           [ "There are two configuration files"
           , "  1. theme.csv"
           , "  2. config.json"
           , ""
           , "  For linux config is in ~/.config/bollama/"
           , ""
           , "The bollama state is stored in a sqlite database."
            , "  For linux this is usually ~/.local/share/bollama/bollama.db"
           ]

       , HlpSection 2 "Theme"
            [ HlpText
                [ "You can change the default theme by creating a file called `theme.csv` in the Bollama config directory."
                , " For linux this is usually ~/.config/bollama/theme.csv"
                , ""
                , "The theme is a CSV file with the following format:"
                ]
            , HlpCode
                [ "  <name>,<fg>,<bg>,<attr>"
                , "  <name>,<fg>,<bg>"
                , "  <name>,<fg>"
                ]
            , HlpText
                [  ""
                , "Where:"
                ]
            , HlpCode
                [ "  <name> is the name of the attribute"
                , "  <fg> is the foreground colour name"
                , "  <bg> is the background colour name"
                , "  <attr> is an optional attribute (bold, dim, reverse, underline)"
                ]
            , HlpText
                [ ""
                , "- Attributes are optional, and one one of"
                , "    - standout"
                , "    - underline"
                , "    - reverseVideo"
                , "    - blink"
                , "    - dim"
                , "    - bold"
                , "    - bright"
                , "    - italic"
                , "    - strikethrough"
                , "    - `-` means default"
                , ""
                , "- colours are either a #hex-code or a name. Known names are show on the `Colours` tab"
                , "    See https://rich.readthedocs.io/en/stable/appendix/colors.html"
                , "- true colour is supported if your terminal supports it and has the correct terminfo "
                , "    (e.g `TERM=tmux-256color` or `TERM=xterm-256color` and `COLORTERM=truecolor`)."
                , "    See https://github.com/termstandard/colors"
                , ""
                , ""
                , "Here is the default theme"
                ]

            , HlpIndent 2 [HlpText [T.defaultTheme]]
            ]

       , HlpSection 2 "Config"
            [ HlpText
                [ "The config is a JSON file."
                , "It looks something like this"
                ]

            , HlpCode
              [ "{"
              , "    \"avoid_emojis\": false,"
              , "    \"default_tab\": \"chat\","
              , "    \"ollama_url\": \"http://localhost:11434\","
              , "    \"allow_mouse\": \"true\","
              , "    \"default_export_dir\": \"/home/user/temp\""
              , "}"
              ]
            ]
        ]
  ]


col :: Int -> Text -> [Char] -> B.Widget n
col width txt' attr =
  (B.vLimit 1 . B.hLimit width $ (B.withAttr (BA.attrName attr) $ B.txt txt') <+> B.fill ' ')
