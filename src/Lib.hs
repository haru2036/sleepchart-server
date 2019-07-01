{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Servant.Server.Experimental.Auth
import Jose.Jwt
import Model



type API = "users" :> Get '[JSON] [User]
        :<|> ProtectedAPI

type ProtectedAPI = AuthProtect "protected" :> Get '[JSON] Text

type instance AuthServerData (AuthProtect "protected") = User

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serveWithContext api authServerContext server

api :: Proxy API
api = Proxy

server :: Server API
server = users :<|> txt

txt :: AuthServerData (AuthProtect "protected") -> Handler Text
txt _ = return "hogehogehoge-"

users :: Handler [User]
users = return [ User "Isaac" "Newton" $ Just 25
        , User "Albert" "Einstein" $ Just 50
        ]

authServerContext :: Context (AuthHandler Request User ': '[])
authServerContext = authHandler :. EmptyContext

authHandler :: AuthHandler Request User 
authHandler = mkAuthHandler handler
  where
    handler req =
        case lookup "X-Servant-Auth-Token" (requestHeaders req) of
            Just sid -> return $ User "hoge" "piyo" $ Just 25
            Nothing  -> throwError $ err401 { errBody = "Missing token header" }
