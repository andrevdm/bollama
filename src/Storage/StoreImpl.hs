{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Storage.StoreImpl
  ( newInMemStore
  , newStoreWrapper
  , newSqliteStore
  , newFileLogger
  ) where

import Verset
import Control.Concurrent.MVar.Strict (MVar', newMVar', modifyMVar', modifyMVar'_, withMVar')
import Control.Concurrent.STM.TVar qualified as TV
import Control.Exception.Safe (finally, catch)
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple qualified as Sq
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as Txt
import Data.Text qualified as Txt
import Data.Time qualified as DT
import Ollama qualified as O
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ (r)

import Messages qualified as M
import Logging qualified as L
import Storage.Store qualified as Sr
import Utils qualified as U


newFileLogger :: FilePath -> IO L.Logger
newFileLogger logPath = do
  pure L.Logger
    { L.lgWarn = \msg -> do
        now <- DT.getCurrentTime
        Txt.appendFile logPath $ show now <> " [WARN] " <> msg
    , L.lgInfo = \msg -> do
        now <- DT.getCurrentTime
        Txt.appendFile logPath $ show now <> " [INFO] " <> msg
    , L.lgDebug = \msg -> do
        now <- DT.getCurrentTime
        Txt.appendFile logPath $ show now <> " [DEBUG] " <> msg
    , L.lgError = \msg -> do
        now <- DT.getCurrentTime
        Txt.appendFile logPath $ show now <> " [ERROR] " <> msg
    , L.lgCritical = \msg -> do
        now <- DT.getCurrentTime
        Txt.appendFile logPath $ show now <> " [CRITICAL] " <> msg
    , L.lgReadLast = \_i -> pure []
    }


newInMemStore :: IO Sr.Store
newInMemStore = do
  store :: TV.TVar (Map M.ChatId (M.Chat, [M.ChatMessage])) <- atomically $ TV.newTVar Map.empty

  pure Sr.Store
    { srListChats = do
        chats <- atomically $ Map.toList <$> TV.readTVar store
        pure $ (\(k, (c, _)) -> c { M.chatId = k }) <$> chats

    , srLoadChat = \chatId -> do
        chats <- atomically $ TV.readTVar store
        pure $ Map.lookup chatId chats

    , srSaveChat = \chat -> do
        unless (Txt.isInfixOf "#" chat.chatName) $ do
          atomically $ TV.modifyTVar' store (Map.insert chat.chatId (chat, []))

    , srSaveChatMessage = \msg -> do
        atomically $ TV.modifyTVar' store $ \m ->
          case Map.lookup msg.msgChatId m of
            Nothing -> m
            Just (c, msgs) ->
              Map.insert msg.msgChatId (c, msg : msgs) m

    , srClearChatInput = \chatId -> do
        atomically $ TV.modifyTVar' store $ \m ->
          case Map.lookup chatId m of
            Nothing -> m
            Just (c, msgs) ->
              Map.insert chatId (c { M.chatInput = "" }, msgs) m

    , srDeleteAllChatMessages = \chatId -> do
        atomically $ TV.modifyTVar' store $ \m ->
          case Map.lookup chatId m of
            Nothing -> m
            Just (c, _) ->
              Map.insert chatId (c {M.chatInput = ""}, []) m

    , srDeleteChat = \chatId -> do
        atomically $ TV.modifyTVar' store $ \m ->
          Map.delete chatId m

    , srGetMessageText = \msgId -> do
        chats <- atomically $ TV.readTVar store
        let ms1 = Map.elems chats
            ms2 = concatMap snd ms1
        case find (\m -> m.msgId == msgId) ms2 of
          Nothing -> pure Nothing
          Just m -> pure $ Just m.msgText
    }


data WrapperState = WrapperState
  { store :: !Sr.Store
  , cache :: !(Map M.ChatId (M.Chat, [M.ChatMessage]))
  , currentId :: !(Maybe M.ChatId)
  }


newStoreWrapper :: (IO (Sr.Store, L.Logger)) -> IO Sr.StoreWrapper
newStoreWrapper mkStore = do
  (storeRaw, logger) <- mkStore

  -- Create a new MVar to hold the state
  --  This ensures that the state is thread-safe and can be modified safely
  --  this includes the cache and access to the underlying store
  --  So we should never use the storeRaw directly
  st <- newMVar' $ WrapperState
    { store = storeRaw
    , cache = Map.empty
    , currentId = Nothing
    }

  --TODO timer to call evict

  let wrapper = Sr.StoreWrapper
       { swListChats = listChats st
       , swNewChat = newChat st
       , swGetChat = getChat st
       , swSetCurrent = \c -> setCurrent st c >>= evict st
       , swGetCurrent = getCurrent st
       , swAddMessage = addMessage st
       , swStreamDone = \c d -> streamDone st c d >>= evict st
       , swAddStreamedChatContent = addStreamedChatContent st
       , swSaveChat = saveChat st
       , swClearChatInput = clearChatInput st
       , swDeleteAllChatMessages = \c -> deleteAllChatMessages st c >>= evict st
       , swDeleteChat = \c -> deleteChat st c >>= evict st
       , swGetMessageText = getMessageText st

       , swLog = logger
       }

  pure wrapper


  where
    deleteChat :: MVar' WrapperState -> M.ChatId -> IO ()
    deleteChat st' chatId = do
      modifyMVar'_ st' $ \st -> do
        st.store.srDeleteChat chatId

        let cache2 = Map.delete chatId st.cache
        pure $ st { cache = cache2 }


    deleteAllChatMessages :: MVar' WrapperState -> M.ChatId -> IO ()
    deleteAllChatMessages st' chatId = do
      modifyMVar'_ st' $ \st -> do
        -- Delete all messages from the store
        st.store.srDeleteAllChatMessages chatId

        -- Remove the chat from the cache
        let cache2 = Map.delete chatId st.cache
        pure $ st { cache = cache2 }


    listChats :: MVar' WrapperState -> IO [M.Chat]
    listChats st' = do
      withMVar' st' $ \st -> do
        -- Get the chats from the store
        stored <- st.store.srListChats
        -- Temp chats are not stored
        let tmp = mapMaybe (\(c, _) -> if Txt.isInfixOf "#" c.chatName then Just c else Nothing) (Map.elems st.cache)
        pure . reverse . sortOn M.chatUpdatedAt $ stored <> tmp


    evict :: MVar' WrapperState -> a -> IO a
    evict st' a = do
      modifyMVar'_ st' $ \st -> do
        let vs2 = Map.filterWithKey
                  (\k (c, _) ->
                    c.chatStreaming == M.SsStreaming
                    || Just k == st.currentId
                    || Txt.isInfixOf "#" c.chatName
                  ) st.cache

        pure $ st { cache = vs2 }
      pure a


    newChat :: MVar' WrapperState -> M.StreamingState -> Text -> Text -> M.ChatParams -> IO M.Chat
    newChat st' streaming chatName'' chatModel'' chatParams = do
      chats <- listChats st'

      -- Always have a unique chat name
      dt' <- DT.getCurrentTime
      let
        dt = Txt.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M:%S" dt'
        chatName' = if (Txt.null . Txt.strip) chatName'' then dt else chatName''
        chatName =
           case find (\c -> c.chatName == chatName') chats of
           Just _ -> chatName' <> "." <> dt
           Nothing -> chatName'

      chatId <- M.ChatId <$> U.newUuidText
      now <- DT.getCurrentTime

      let chat = M.Chat
            { M.chatId = chatId
            , M.chatName = chatName
            , M.chatCreatedAt = now
            , M.chatUpdatedAt = now
            , M.chatModel = chatModel''
            , M.chatParams = chatParams
            , M.chatStreaming = streaming
            , M.chatInput = ""
            }

      modifyMVar'_ st' $ \st -> do
        let cache2 = Map.insert chatId (chat, []) st.cache

        -- Store chats, unless there are temporary chats
        unless (Txt.isInfixOf "#" chatName) $
          st.store.srSaveChat chat

        pure st { cache = cache2 }

      pure chat

    saveChat :: MVar' WrapperState -> M.Chat -> IO ()
    saveChat st' chat = do
      modifyMVar'_ st' $ \st -> do
        -- Update the cache with the new chat
        let cache2 = Map.adjust (\(c1, ms) ->
             let c2 = c1 { M.chatName = chat.chatName
                         , M.chatModel = chat.chatModel
                         , M.chatUpdatedAt = chat.chatUpdatedAt
                         , M.chatStreaming = chat.chatStreaming
                         , M.chatInput = chat.chatInput
                         }
             in
             (c2, ms)) chat.chatId st.cache

        unless (Txt.isInfixOf "#" chat.chatName) $ do
          -- Save the new chat to the store
          _ <- st.store.srSaveChat chat
          pass

        pure st { cache = cache2 }


    getChat :: MVar' WrapperState -> M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage]))
    getChat st' chatId = do
      withMVar' st' $ \st -> getChat' st chatId

    getChat' :: WrapperState -> M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage]))
    getChat' st chatId = do
      case Map.lookup chatId st.cache of
        Just (chat, ms) -> pure $ Just (chat, reverse ms)
        Nothing -> do
          st.store.srLoadChat chatId >>= \case
            Nothing -> pure Nothing
            Just (chat, ms) -> pure $ Just (chat, reverse ms)


    setCurrent :: MVar' WrapperState -> Maybe M.ChatId -> IO (Maybe (M.Chat, [M.ChatMessage]))
    setCurrent st' chatId' = do
      modifyMVar' st' $ \st -> do
        let st2 = st { currentId = chatId' }
        res <-
          case chatId' of
            Nothing -> pure Nothing
            Just chatId -> getChat' st chatId
        pure (st2, res)


    getCurrent :: MVar' WrapperState -> IO (Maybe (M.ChatId, M.Chat, [M.ChatMessage]))
    getCurrent st' = do
      modifyMVar' st' $ \st -> do
        case st.currentId of
          -- No current chat
          Nothing -> pure (st, Nothing)
          -- Current chat is set
          Just currentId -> do
            case Map.lookup currentId st.cache of
              -- Current chat is in cache
              Just (chat, ms) ->
                pure (st, Just (currentId, chat, reverse ms))

              -- Current chat is not in cache
              Nothing -> do
                st.store.srLoadChat currentId >>= \case
                  -- Current chat is not in store
                  Nothing ->
                    pure (st, Nothing)

                  -- Current chat is in store
                  Just (c, ms) -> do
                    -- Update cache with the loaded chat
                    let st2 = st { cache = Map.insert currentId (c, ms) st.cache }
                    -- done
                    pure (st2, Just (currentId, c, reverse ms))




    addMessage :: MVar' WrapperState -> M.ChatId -> O.Role -> M.StreamingState -> Text -> Text -> IO (Either Text M.ChatMessage)
    addMessage st' chatId role streamingSts model text = do
      now <- DT.getCurrentTime
      mid <- M.MessageId <$> U.newUuidText
      let newMsg = M.ChatMessage
            { M.msgChatId = chatId
            , M.msgId = mid
            , M.msgRole = role
            , M.msgModel = model
            , M.msgCreatedAt = now
            , M.msgText = text
            , M.msgDetail = Nothing
            }

      modifyMVar'_ st' $ \st -> do
        -- Update the cache with the new message, if it is in the cache
        let cache2 =
             Map.adjust
              (\(chat, ms1) ->
                let ms2 = newMsg : ms1
                    chat2 = chat { M.chatUpdatedAt = now, M.chatStreaming = streamingSts }
                in
                (chat2, ms2)
                )
              chatId
              st.cache

        -- Save the new message to the store
        _ <- st.store.srSaveChatMessage newMsg
        pure st { cache = cache2 }

      pure . Right $ newMsg


    addStreamedChatContent :: MVar' WrapperState -> M.ChatId -> M.MessageId -> O.Role -> Text -> IO (Either Text ())
    addStreamedChatContent st' chatId msgId role text = do
      modifyMVar' st' $ \st -> do
        current' <-
          -- Get chat from cache or store
          case Map.lookup chatId st.cache of
            Just (chat, ms) -> pure $ Just (chat, ms)
            Nothing -> do
              st.store.srLoadChat chatId >>= \case
                Nothing -> pure Nothing
                Just (chat, ms) -> do
                  pure $ Just (chat, ms)

        case current' of
          Nothing -> pure (st, Left $ "Chat not found in cache or store: " <> show chatId)
          Just (chat, ms1) -> do
            (current1, rest) <-
              case ms1 of
                (m : rest') | m.msgId == msgId -> pure (m, rest')
                _ -> do
                  now <- DT.getCurrentTime
                  pure ( M.ChatMessage
                           { M.msgChatId = chatId
                           , M.msgId = msgId
                           , M.msgRole = role
                           , M.msgModel = ""
                           , M.msgCreatedAt = now
                           , M.msgText = ""
                           , M.msgDetail = Nothing
                           }
                       , ms1)

            let current2 = current1 {M.msgText = M.msgText current1 <> text}
            let st2 = st { cache = Map.insert chatId (chat, current2 : rest) st.cache }
            pure (st2, Right ())


    streamDone :: MVar' WrapperState -> M.ChatId -> Maybe M.MessageDetail -> IO ()
    streamDone st' chatId detail = do
      modifyMVar'_ st' $ \st -> do
        case Map.lookup chatId st.cache of
          Nothing -> pass
          Just (_, []) -> pass
          Just (_, (m:_)) -> st.store.srSaveChatMessage $ m { M.msgDetail = detail }

        let cache2 = Map.adjust go chatId st.cache
        pure st { cache = cache2 }

      where
        go (chat, ms) = (chat{ M.chatStreaming = M.SsNotStreaming }, ms)


    clearChatInput :: MVar' WrapperState -> M.ChatId -> IO (Either Text ())
    clearChatInput st' chatId = do
      modifyMVar' st' $ \st -> do
        case Map.lookup chatId st.cache of
          Nothing -> pure (st, Right ())
          Just (chat, ms) -> do
            let chat2 = chat { M.chatInput = "" }
            let st2 = st { cache = Map.insert chatId (chat2, ms) st.cache }
            pure (st2, Right ())


    getMessageText :: MVar' WrapperState -> M.MessageId -> IO (Maybe Text)
    getMessageText st' msgId = do
      withMVar' st' $ \st -> do
        let ms1 = Map.elems st.cache
            ms2 = concatMap snd ms1
        case find (\m -> m.msgId == msgId) ms2 of
          Just m -> pure $ Just m.msgText
          Nothing -> st.store.srGetMessageText msgId


data MsgData = MsgData
  { mdId :: !Text
  , mdRole :: !Text
  , mdModel :: Text
  , mdCreatedAt :: !DT.UTCTime
  , mdMsg :: !Text
  , mdTotalDuration :: !(Maybe Int)
  , mdLoadDuration :: !(Maybe Int)
  , mdPromptEvalCount :: !(Maybe Int)
  , mdPromptEvalDuration :: !(Maybe Int)
  , mdEvalCount :: !(Maybe Int)
  , mdEvalDuration :: !(Maybe Int)
  } deriving stock (Show, Eq, Generic)

instance Sq.FromRow MsgData where
    fromRow = MsgData
      <$> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field
      <*> Sq.field



newSqliteStore :: FilePath -> (L.LogLevel -> Text -> IO ()) -> IO (Sr.Store, L.Logger)
newSqliteStore dbPath onLog = do
  _ <- initDb
  now <- DT.getCurrentTime
  let sessionId = Txt.pack $ DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

  withConn_ $ \conn -> do
    -- TODO configure expires
    Sq.execute_ conn "DELETE FROM log WHERE createdAt < datetime('now', '-2 days')"

  let store = Sr.Store
       { srListChats = do
           withConn $ \conn -> do
             chats :: [(Text, Text, Text, DT.UTCTime, DT.UTCTime, Maybe Int, Maybe Double, Maybe Text)] <-
               Sq.query_ conn "SELECT id, name, model, createdAt, updatedAt, prm_num_ctx, prm_temperature, chatInput FROM chat order by name"

             pure $ chats <&> \(chatId, chatName, chatModel, createdAt, updatedAt, num_ctx, temp, chatInput) ->
               M.Chat
                 { M.chatId = M.ChatId chatId
                 , M.chatName = chatName
                 , M.chatCreatedAt = createdAt
                 , M.chatUpdatedAt = updatedAt
                 , M.chatModel = chatModel
                 , M.chatParams = M.ChatParams
                     { M._cpTemp = temp
                     , M._cpContextSize = num_ctx
                     }
                 , M.chatStreaming = M.SsNotStreaming
                 , M.chatInput = fromMaybe "" chatInput
                 }

       , srLoadChat = \(M.ChatId chatId) -> do
           withConn $ \conn -> do
             chat1 :: [(Text, Text, Text, DT.UTCTime, DT.UTCTime, Maybe Int, Maybe Double, Maybe Text)] <-
               Sq.query conn "SELECT id, name, model, createdAt, updatedAt, prm_num_ctx, prm_temperature, chatInput FROM chat WHERE id = ?" (Sq.Only chatId)

             case chat1 of
               [] -> pure Nothing
               ((id, name, model, createdAt, updatedAt, num_ctx, temp, chatInput): _) -> do
                 let chat =
                       M.Chat
                         { M.chatId = M.ChatId id
                         , M.chatName = name
                         , M.chatCreatedAt = createdAt
                         , M.chatUpdatedAt = updatedAt
                         , M.chatModel = model
                         , M.chatParams = M.ChatParams
                             { M._cpTemp = temp
                             , M._cpContextSize = num_ctx
                             }
                         , M.chatStreaming = M.SsNotStreaming
                         , M.chatInput = fromMaybe "" chatInput
                         }

                 msgs1 :: [MsgData] <-
                   Sq.query conn "SELECT id, role, model, createdAt, msg, totalDuration, loadDuration, promptEvalCount, promptEvalDuration, evalCount, evalDuration FROM chatMessage WHERE chatId = ? order by createdAt desc" (Sq.Only chatId)

                 let msgs =
                       msgs1 <&> \md ->
                         M.ChatMessage
                           { M.msgChatId = M.ChatId chatId
                           , M.msgId = M.MessageId md.mdId
                           , M.msgModel = md.mdModel
                           , M.msgCreatedAt = md.mdCreatedAt
                           , M.msgText = md.mdMsg
                           , M.msgRole =
                               case Txt.toLower md.mdRole of
                                 "system" -> O.System
                                 "assistant" -> O.Assistant
                                 "tool" -> O.Tool
                                 _ -> O.User
                           , M.msgDetail = Just M.MessageDetail
                               { M.cdTotalDuration = md.mdTotalDuration
                               , M.cdLoadDuration = md.mdLoadDuration
                               , M.cdPromptEvalCount = md.mdPromptEvalCount
                               , M.cdPromptEvalDuration = md.mdPromptEvalDuration
                               , M.cdEvalCount = md.mdEvalCount
                               , M.cdEvalDuration = md.mdEvalDuration
                               }
                           }

                 pure $ Just (chat, msgs)


       , srSaveChat = \chat -> do
           withConn_ $ \conn -> do
             Sq.execute conn "INSERT OR REPLACE INTO chat (id, name, model, createdAt, updatedAt, prm_num_ctx, prm_temperature, chatInput) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
               ( chat.chatId & \(M.ChatId chatId) -> chatId
               , chat.chatName
               , chat.chatModel
               , chat.chatCreatedAt
               , chat.chatUpdatedAt
               , chat.chatParams._cpContextSize
               , chat.chatParams._cpTemp
               , chat.chatInput
               )

       , srSaveChatMessage = \msg -> do
           withConn_ $ \conn -> do
             Sq.executeNamed conn
               [r| INSERT OR REPLACE INTO chatMessage
                     (id, chatId, role, model, createdAt, msg, totalDuration, loadDuration, promptEvalCount, promptEvalDuration, evalCount, evalDuration)
                   VALUES
                     (:id, :chatId, :role, :model, :createdAt, :msg, :totalDuration, :loadDuration, :promptEvalCount, :promptEvalDuration, :evalCount, :evalDuration)
               |]
              [ ":id" := (msg.msgId & \(M.MessageId msgId) -> msgId)
              , ":chatId" := (msg.msgChatId & \(M.ChatId chatId) -> chatId)
              , ":role" := (show @_ @Text msg.msgRole)
              , ":model" := msg.msgModel
              , ":createdAt" := msg.msgCreatedAt
              , ":msg" := msg.msgText
              , ":totalDuration" := (M.cdTotalDuration <$> msg.msgDetail)
              , ":loadDuration" := (M.cdLoadDuration <$> msg.msgDetail)
              , ":promptEvalCount" := (M.cdPromptEvalCount <$> msg.msgDetail)
              , ":promptEvalDuration" := (M.cdPromptEvalDuration <$> msg.msgDetail)
              , ":evalCount" := (M.cdEvalCount <$> msg.msgDetail)
              , ":evalDuration" := (M.cdEvalDuration <$> msg.msgDetail)
              ]

       , srClearChatInput = \chatId -> do
            withConn_ $ \conn -> do
              Sq.execute conn "UPDATE chat SET chatInput = ? WHERE id = ?" (Nothing :: Maybe Text, chatId & \(M.ChatId cid) -> cid)

       , srDeleteChat = \chatId -> do
            withConn_ $ \conn -> do
              Sq.execute conn "DELETE FROM chatMessage WHERE chatId = ?" (chatId & \(M.ChatId cid) -> (Sq.Only cid))
              Sq.execute conn "DELETE FROM chat WHERE id = ?" (chatId & \(M.ChatId cid) -> (Sq.Only cid))

       , srDeleteAllChatMessages = \chatId -> do
            withConn_ $ \conn -> do
              Sq.execute conn "DELETE FROM chatMessage WHERE chatId = ?" (chatId & \(M.ChatId cid) -> (Sq.Only cid))
              Sq.execute conn "update chat set chatInput = null WHERE id = ?" (chatId & \(M.ChatId cid) -> (Sq.Only cid))

       , srGetMessageText = \msgId -> do
            withConn $ \conn -> do
              msgs :: [Sq.Only Text] <-
                Sq.query conn "SELECT msg FROM chatMessage WHERE id = ?" (Sq.Only $ msgId & \(M.MessageId mid) -> mid)

              pure $ case msgs of
                [] -> Nothing
                (Sq.Only msg:_) -> Just msg
       }

  let logger =
       L.Logger
         { L.lgWarn = \t -> insertLog sessionId L.LlWarn t >> onLog L.LlWarn t
         , L.lgInfo = \t -> insertLog sessionId L.LlInfo t >> onLog L.LlInfo t
         , L.lgDebug = \t -> insertLog sessionId L.LlDebug t >> onLog L.LlDebug t
         , L.lgError = \t -> insertLog sessionId L.LlError t >> onLog L.LlError t
         , L.lgCritical = \t -> insertLog sessionId L.LlCritical t >> onLog L.LlCritical t
         , L.lgReadLast = readLogs sessionId
         }


  pure (store, logger)

  where
    initDb :: IO ()
    initDb = do
      withConn_ $ \conn -> do
        _ <- liftIO . Sq.execute_ conn . Sq.Query $ Txt.unlines
          [ "pragma journal_mode = WAL;"
          , "pragma synchronous = normal;"
          , "pragma temp_store = memory;"
          , "pragma mmap_size = 30000000000;"
          , "pragma journal_size_limit = 6144000;"
          ]

        traverse_ (Sq.execute_ conn) createScripts
        traverse_ (migrate conn) migrationScripts

    migrate :: Sq.Connection -> (Text, Text) -> IO ()
    migrate conn (version, query) = do
      res :: [Sq.Only Text] <- Sq.query conn "SELECT id FROM migrate WHERE id = ?" (Sq.Only version)
      when (null res) $ do
        Sq.withTransaction conn $ do
          let ls = filter (not . Txt.null) $ Txt.strip <$> Txt.lines query
          for_ ls $ \l -> do
            Sq.execute_ conn $ Sq.Query l

          Sq.execute conn "INSERT INTO migrate (id) VALUES (?)" (Sq.Only version)


    insertLog sessionId level' msg = do
      catch
        (do
          let level = fromEnum level'
          now <- DT.getCurrentTime
          withConn_ $ \conn -> do
            Sq.execute conn "INSERT INTO log (sessionId, level, createdAt, msg) VALUES (?, ?, ?, ?)" (sessionId, level, now, msg)
        )
        (\(e :: SomeException) -> do
          onLog L.LlCritical $ "Failed to insert log: \b" <> show e <> "\n\nOriginal log:\n" <> msg
          pPrint e
          pure ()
        )


    readLogs :: Text -> Int -> IO [L.LogEntry]
    readLogs sessionId n = do
      withConn $ \conn -> do
        logs :: [(Int, DT.UTCTime, Text)] <-
          Sq.query conn "SELECT level, createdAt, msg FROM log where sessionId = ? ORDER BY createdAt DESC LIMIT ?" (sessionId, n)

        pure $ logs <&> \(level, createdAt, msg) ->
          L.LogEntry
            { L.leLevel = fromMaybe L.LlError $ toEnumMay level
            , L.leTime = createdAt
            , L.leText = msg
            }


    withConn :: (Sq.Connection -> IO a) -> IO a
    withConn f = do
      conn <- Sq.open dbPath
      finally (f conn) (Sq.close conn)


    withConn_ :: (Sq.Connection -> IO a) -> IO ()
    withConn_ = void . withConn


    createScripts :: [Sq.Query]
    createScripts =
     [ [r| CREATE TABLE IF NOT EXISTS chat (
             id                    TEXT      PRIMARY KEY,
             name                  TEXT      NOT NULL,
             createdAt             DATETIME  NOT NULL,
             updatedAt             DATETIME  NOT NULL,
             model                 TEXT      NOT NULL,
             prm_num_ctx           INT       NULL,
             prm_temperature       FLOAT     NULL,
             prm_mirostat          INT       NULL,
             prm_mirostat_eta	     FLOAT     NULL,
             prm_mirostat_tau	     FLOAT     NULL,
             prm_repeat_last_n     INT       NULL,
             prm_repeat_penalty    FLOAT     NULL,
             prm_seed	             INT       NULL,
             prm_num_predict       INT       NULL,
             prm_top_k             INT       NULL,
             prm_top_p             FLOAT     NULL,
             prm_min_p             FLOAT     NULL
           );
       |]
     , [r| CREATE TABLE IF NOT EXISTS chatMessage (
             id         TEXT      PRIMARY KEY,
             chatId     TEXT      NOT NULL,
             role       TEXT      NOT NULL,
             model      TEXT      NOT NULL,
             createdAt  DATETIME  NOT NULL,
             msg        TEXT      NOT NULL,
             FOREIGN KEY (chatId) REFERENCES chat(chatId) ON DELETE CASCADE
           );
       |]

     , [r| CREATE TABLE IF NOT EXISTS log (
             id         INTEGER   PRIMARY KEY AUTOINCREMENT,
             sessionId  TEXT      NOT NULL,
             level      INTEGER   NOT NULL,
             createdAt  DATETIME  NOT NULL,
             msg        TEXT      NOT NULL
           );
       |]

     , [r| CREATE TABLE IF NOT EXISTS migrate (
             id         text      PRIMARY KEY
           );
       |]

     , "CREATE INDEX IF NOT EXISTS idx_chat_message_chat_id ON chatMessage(chatId);"
     , "CREATE INDEX IF NOT EXISTS idx_chat_message_created_at ON chatMessage(createdAt);"
     , "CREATE INDEX IF NOT EXISTS ix_log_created_at ON log(createdAt);"
     , "CREATE INDEX IF NOT EXISTS ix_log_created_at ON log(sessionId);"
     ]


    migrationScripts :: [(Text, Text)]
    migrationScripts =
      [  ( "0001"
         , [r| alter table chat add column chatInput text null;
           |]
         )

      , ( "0002.message detail"
         , [r| alter table chatMessage add column totalDuration integer null;
               alter table chatMessage add column loadDuration integer null;
               alter table chatMessage add column promptEvalCount integer null;
               alter table chatMessage add column promptEvalDuration integer null;
               alter table chatMessage add column evalCount integer null;
               alter table chatMessage add column evalDuration integer null;
           |]
         )
      ]
