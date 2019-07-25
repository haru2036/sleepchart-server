
{-# LANGUAGE TemplateHaskell #-}
module Api.User where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Servant
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Model

data RegisterResult = RegSuccess | AlreadyTaken
      deriving(Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''RegisterResult)

registerUser :: ConnectionPool -> User -> Handler RegisterResult
registerUser pool user = liftIO $ flip runSqlPool pool $ do
      mUser <- getByValue user
      case mUser of
            Just user -> return AlreadyTaken
            Nothing -> insert user >> return RegSuccess
