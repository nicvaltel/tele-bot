{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

newtype BotApp = BotApp {unBotApp :: PG.AppState}

runBot :: FilePath -> IO ()
runBot cfgFile = do
  Right pgCfg <- PG.readDBConfig cfgFile
  PG.withAppState pgCfg $ \pool ->
    B.botStartup (PG.getToken pgCfg) (handleWeightControl $ BotApp pool)


instance M.BotDBModel BotApp where
  getUserById pool uId = runReaderT (PG.getUserById uId) (unBotApp pool)
  insertMsg pool uId txt = runReaderT (PG.insertMsg uId txt) (unBotApp pool)
  createUser pool uId uName = runReaderT (PG.createUser uId uName) (unBotApp pool)




msgHello, msgInstruction, msgSave :: Text
msgHello = "Я бот-запоминатель веса. Напишите вес, чтобы я сохранил его."
msgInstruction = "Просто введите цифрами ваш вес в кг, и я запомню его"
msgSave = "Сохранил ваш вес - " 

handleWeightControl :: BotApp -> B.Action -> B.ChatModel -> Eff B.Action B.ChatModel
handleWeightControl pool action model = traceShow action $
  case action of
    B.NoAction -> pure model
    B.RecordMsg usrId mayUsrname _ word -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ M.getUserById pool usrId
        when (isNothing maybeUser) $ liftIO $ M.createUser pool usrId usrname >> pure ()
        _ <- liftIO $ M.insertMsg pool usrId word
        case maybeUser of
          Just _ -> do
            msgSaveResult <- liftIO $ M.insertMsg pool usrId word
            case msgSaveResult of
              Right (Right w) -> replyString $ msgSave <> pack (show w)
              Right (Left _) -> replyString msgInstruction
              Left err -> replyString . pack $ show err
          Nothing -> replyString msgHello
        pure B.NoAction
  where
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage

