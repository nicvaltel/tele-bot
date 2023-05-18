{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Bot
  ( botStartup,
    ChatModel (..),
    Action (..),
    handleWeightControl,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import qualified Domain.Model as M
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel
  = ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)

botStartup :: (MonadIO m) => String -> (Action -> ChatModel -> Eff Action ChatModel) -> m ()
botStartup tokenStr handleAction = do
  let token = Token . pack $ tokenStr
  env <- liftIO $ defaultTelegramClientEnv token
  liftIO $ startBot_ (conversationBot updateChatId (incexpBotApp handleAction)) env

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate

incexpBotApp :: (Action -> ChatModel -> Eff Action ChatModel) -> BotApp ChatModel Action
incexpBotApp handleAction = BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate _ update = do
  msg <- updateMessage update
  usr <- messageFrom msg
  let Telegram.UserId usrId = Telegram.userId usr
  let Telegram.MessageId msgId = Telegram.messageMessageId msg
  let usrIdInt = fromIntegral usrId :: Int
  let msgIdInt = fromIntegral msgId :: Int
  let usrName = Telegram.userUsername usr
  let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
  parseUpdate parser update

msgHello, msgInstruction, msgSave :: Text
msgHello = "Я бот-запоминатель веса. Напишите вес, чтобы я сохранил его."
msgInstruction = "Просто введите цифрами ваш вес в кг, и я запомню его"
msgSave = "Сохранил ваш вес - "

handleWeightControl :: M.BotDBModel a => a -> Action -> ChatModel -> Eff Action ChatModel
handleWeightControl pool action model = traceShow action $
  case action of
    NoAction -> pure model
    RecordMsg usrId mayUsrname _ word -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ M.getUserById pool usrId
        when (isNothing maybeUser) $ liftIO $ M.createUser pool usrId usrname >> pure ()
        case maybeUser of
          Just _ -> do
            msgSaveResult <- liftIO $ M.insertMsg pool usrId word
            case msgSaveResult of
              Right (Right w) -> replyString $ msgSave <> pack (show w)
              Right (Left _) -> replyString msgInstruction
              Left err -> replyString . pack $ show err
          Nothing -> replyString msgHello
        pure NoAction
  where
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage
