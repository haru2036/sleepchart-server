{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( startApp
    , app
    ) where

import Protolude hiding(fromStrict)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.List (find, lookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Servant.Server.Experimental.Auth
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet, decodeCompact, defaultJWTValidationSettings, verifyClaims, claimSub, FromCompact, AsError, StringOrURI)
import Crypto.JOSE.JWK (JWK, fromOctets)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans
import Control.Monad.Except (catchError)
import Control.Lens ((^.))
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
server = (return users) :<|> txt

txt :: AuthServerData (AuthProtect "protected") -> Handler Text
txt _ = return "hogehogehoge-"

users :: [User]
users = [ User "Isaac" "" $ Just 25
        , User "Albert" "Einstein" $ Just 50
        ]

authServerContext :: Context (AuthHandler Request User ': '[])
authServerContext = authHandler :. EmptyContext

authHandler :: AuthHandler Request User 
authHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler User
    handler req =
        case lookup "X-Servant-Auth-Token" (requestHeaders req) of
            Nothing  -> throwError $ err401 { errBody = "Missing token header" }
            Just sid -> do
                        let jsonJwk = "" :: ByteString
                        let jwk = fromOctets jsonJwk
                        eithersignedjwt <- runExceptT $ do
                          decodeCompact (fromStrict sid) :: (FromCompact a, MonadError JWTError m) => m a 
                        case eithersignedjwt of
                          Right (signedjwt :: SignedJWT) -> do
                            result <- liftIO $ doJwtVerify jwk signedjwt
                            case result of
                              Right claimsSet -> do
                                let a = claimsSet ^. claimSub 
                                {- case claimsSet ^. claimSub >>= (\sub -> find (\item -> ((show (sub :: StringOrURI)) :: String) == ((show $ userUid $ item)) :: String) users) of
                                  Just usr -> return usr
                                  Nothing -> throwError $ err401 { errBody = "user not found" }
                                  -}
                                return $ User "hoge" "piyo" $ Just 25
                              Left err -> throwError $ err401 { errBody = "invalid claim" }
                          Left err -> throwError $ err401 { errBody = "jwt decode error" }
                        where
                          doJwtVerify :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
                          doJwtVerify jwk jwt = runExceptT $ do
                            let config = defaultJWTValidationSettings (== "bob")
                            verifyClaims config jwk jwt


