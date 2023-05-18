{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Model
  ( BotDBModel (..),
    UserId,
    Username,
    MessageId,
    User (..),
    Message (..),
    MessageError (..),
    isMessageWeight,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Time (UTCTime)
import GHC.Float (double2Float)

type UserId = Int

type Username = Text

type MessageId = Int

data User = User
  { userId :: UserId,
    username :: Username,
    created :: UTCTime
  }
  deriving (Show, Eq)

data Message = Message
  { messageId :: MessageId,
    uId :: Int,
    text :: Text,
    sent :: UTCTime
  }
  deriving (Show, Eq)

newtype MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

class BotDBModel a where
  getUserById :: a -> UserId -> IO (Maybe User)
  insertMsg :: a -> UserId -> Text -> IO (Either MessageError (Either Text Float))
  createUser :: a -> UserId -> Username -> IO User

isMessageWeight :: Text -> Maybe Float
isMessageWeight msg =
  case T.double msg of
    Right (w, "") -> Just $ double2Float w
    _ -> case T.double (T.map (\c -> if c == ',' then '.' else c) msg) of
      Right (w, "") -> Just $ double2Float w
      _ -> Nothing
