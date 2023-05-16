{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( runBot,
  )
where

import qualified Adapter.PostgreSQL.Adapter as PG
import qualified Adapter.PostgreSQL.Common as PG
import Control.Monad (when)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import qualified Domain.Bot as B
import qualified Domain.Model as M
import Telegram.Bot.Simple (BotM, Eff, reply, toReplyMessage, (<#))

runBot :: FilePath -> IO ()
runBot cfgFile = do
  Right pgCfg <- PG.readDBConfig cfgFile
  PG.withAppState pgCfg $ \pool ->
    B.botStartup (PG.getToken pgCfg) (handleWeightControl pool)

getUserById_ :: PG.AppState -> M.UserId -> IO (Maybe M.User)
getUserById_ pool uId = runReaderT (PG.getUserById uId) pool

insertMsg_ :: PG.AppState -> M.UserId -> Text -> IO (Either M.MessageError (Either Text Float))
insertMsg_ pool uId txt = runReaderT (PG.insertMsg uId txt) pool

createUser_ :: PG.AppState -> M.UserId -> M.Username -> IO M.User
createUser_ pool uId uName = runReaderT (PG.createUser uId uName) pool

handleWeightControl :: PG.AppState -> B.Action -> B.ChatModel -> Eff B.Action B.ChatModel
handleWeightControl pool action model = traceShow action $
  case action of
    B.NoAction -> pure model
    B.RecordMsg usrId mayUsrname _ word -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ getUserById_ pool usrId
        when (isNothing maybeUser) $ liftIO $ createUser_ pool usrId usrname >> pure ()
        _ <- liftIO $ insertMsg_ pool usrId word
        case maybeUser of
          Just _ -> do
            msgSaveResult <- liftIO $ insertMsg_ pool usrId word
            case msgSaveResult of
              Right (Right w) -> replyString $ "Сохранил ваш вес - " <> pack (show w) <> " Охренеть вы жирный!"
              Right (Left _) -> replyString "Да что вы говорите! Очень интересно..."
              Left err -> replyString . pack $ show err
          Nothing -> replyString "Я бот-запоминатель веса. Напишите вес, чтобы я сохранил его."
        pure B.NoAction
  where
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage
