{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Bot
  ( botStartup,
    ChatModel (..),
    Action (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text
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
