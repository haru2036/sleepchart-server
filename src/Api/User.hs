
{-# LANGUAGE TemplateHaskell #-}
module Api.User where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Servant
import Database.Persist
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Model

data RegisterMessage = RegSuccess | AleardyTaken | TokenError
      deriving(Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''RegisterMessage)
