{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.Adapter
  ( getUserById,
    createUser,
    insertMsg,
  )
where

import Adapter.PostgreSQL.Common (PG, withConn)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Only (Only), query)
import Domain.Model
  ( MessageError (..),
    User (..),
    UserId,
    Username,
  )
import qualified Domain.Model as M
import UnliftIO (throwString)

getUserById :: PG r m => UserId -> m (Maybe User)
getUserById uid = do
  result :: [(Int, Text, UTCTime)] <- withConn $ \conn -> query conn qryStr (Only uid)
  case result of
    [] -> pure Nothing
    [(userId, username, created)] -> pure $ Just User {userId, username, created}
    _ -> throwString $ "Should not happen: several userId's in users table in DB - id = " ++ show uid
  where
    qryStr = "select user_id, username, created from weight_control.users where user_id = ?"

createUser :: PG r m => UserId -> Username -> m User
createUser userId username = do
  result :: [Only UTCTime] <- withConn $ \conn -> query conn qryStr (userId, username)
  case result of
    [Only created] -> pure User {userId, username, created}
    _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show username
  where
    qryStr = "insert into weight_control.users (user_id, username) values (?, ?) returning created"

insertMsg :: PG r m => UserId -> Text -> m (Either MessageError (Either Text Float))
insertMsg uId text = do
  mayUser <- getUserById uId
  case mayUser of
    Nothing -> pure (Left $ UserDoesNotExist uId)
    Just _ ->
      case M.isMessageWeight text of
        Just w -> do
          resultMsg :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStrMsg (uId, text, True)
          resultWght :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStrWght (uId, w)
          do
            case resultMsg of
              [(_, _)] -> pure ()
              _ -> throwString $ "Should not happen: cannot create message in messages table in DB - userId = " ++ show uId ++ " text = " ++ show text
            case resultWght of
              [(_, _)] -> pure ()
              _ -> throwString $ "Should not happen: cannot create weight in weight table in DB - userId = " ++ show uId ++ " text = " ++ show text
            pure (Right (Right w))
        Nothing -> do
          resultMsg :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStrMsg (uId, text, False)
          case resultMsg of
            [(_, _)] -> pure $ Right (Left text)
            _ -> throwString $ "Should not happen: cannot create message in messages table in DB - userId = " ++ show uId ++ " text = " ++ show text
  where
    qryStrMsg = "insert into weight_control.messages (user_id, text, is_weight) values (?,?,?) returning id, sent"
    qryStrWght = "insert into weight_control.weight (user_id, weight) values (?,?) returning id, sent"
