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
    ) where

import Protolude hiding(fromStrict, readFile)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, words, pack)
import Data.ByteString (ByteString, readFile)
import Data.ByteString.Lazy (fromStrict)
import Data.List (find, lookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Servant.Auth.Server
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet, stringOrUri, decodeCompact, defaultJWTValidationSettings, verifyClaims, claimSub, FromCompact, AsError, StringOrURI, JWTValidationSettings)
import Crypto.JOSE.JWK (JWK, fromOctets, JWKSet(..))
import Crypto.JOSE.JWA.JWS (Alg(..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans
import Control.Monad.Except (catchError)
import Control.Lens ((^.))
import Model


type API auths  = "users" :> Get '[JSON] [User]
        :<|> (Servant.Auth.Server.Auth auths User :> ProtectedAPI)

type ProtectedAPI = Auth '[JWT, Cookie] User :> "protected" :> Get '[JSON] Text

protected :: Servant.Auth.Server.AuthResult User -> Server ProtectedAPI
protected (Servant.Auth.Server.Authenticated user) = return $ txt user
protected _ =  throwAll err401

data AuthResult val
  = BadPassword
  | NoSuchUser
  | Authenticated val
  | Indefinite

startApp :: IO ()
startApp = do
  -- We generate the key for signing tokens. This would generally be persisted,
  -- and kept safely
  -- Adding some configurations. All authentications require CookieSettings to
  -- be in the context.
  jsonJwk <- readFile "./jwk.json" 
  let Just (Success jwkset) = fromJSON <$> decode (fromStrict jsonJwk)
  let jwk = fromOctets jsonJwk
  Just trustedAudiences <- decode . fromStrict <$> readFile "./audience.json"
  let jwtCfg = JWTSettings jwk (Just RS256) jwkset (matchAud trustedAudiences)
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      --- Here we actually make concrete
      api = Proxy :: Proxy (API '[JWT])
      matchAud :: [StringOrURI] -> StringOrURI -> IsMatch
      matchAud trusteds aud = case find (== aud) trusteds of
                                Just _ -> Matches
                                Nothing -> DoesNotMatch
  putStrLn ("starting server at port 8080" :: Text)
  run 8080 $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

api :: Proxy (API '[JWT])
api = Proxy

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cs jwts = (return users) :<|> protected

txt :: User -> Handler Text
txt user = return $ pack $ show user

users :: [User]
users = [ User "Isaac" "" $ Just 25
        , User "Albert" "Einstein" $ Just 50
        ]
