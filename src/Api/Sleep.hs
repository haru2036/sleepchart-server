{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Sleep where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

type SleepAPI = 
         "sleeps" :> Get '[JSON] [Entity SleepSession]
    :<|> "sleeps" :> ReqBody '[JSON] SleepSession :> Post '[JSON] Int64

sleepServer :: ServerT SleepAPI App
sleepServer = allSleeps :<|> createSleep

allSleeps :: App [Entity SleepSession]
allSleeps =
    runDb (selectList [] [])

createSleep :: SleepSession -> App Int64
createSleep p = do
    newUser <- runDb (insert (SleepSession (sleepSessionStart p) (sleepSessionEnd p)))
    return $ fromSqlKey newUser
