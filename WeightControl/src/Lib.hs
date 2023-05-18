{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
  ( runBot,
  )
where

import qualified Adapter.PostgreSQL.Adapter as PG
import qualified Adapter.PostgreSQL.Common as PG
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Domain.Bot as B
import qualified Domain.Model as M

newtype BotLib = BotLib {unBotApp :: PG.AppState}

runBot :: FilePath -> IO ()
runBot cfgFile = do
  Right pgCfg <- PG.readDBConfig cfgFile
  PG.withAppState pgCfg $ \pool ->
    B.botStartup (PG.getToken pgCfg) (B.handleWeightControl $ BotLib pool)

instance M.BotDBModel BotLib where
  getUserById pool uId = runReaderT (PG.getUserById uId) (unBotApp pool)
  insertMsg pool uId txt = runReaderT (PG.insertMsg uId txt) (unBotApp pool)
  createUser pool uId uName = runReaderT (PG.createUser uId uName) (unBotApp pool)
