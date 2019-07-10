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
import Database.Persist.Sql
import DataStore.Internal
import Model
import Api.Sleep


type API auths  = (Servant.Auth.Server.Auth auths User :> ProtectedAPI)

type ProtectedAPI = "protected" :> "sleeps" :> Get '[JSON] [Sleep]
               :<|> "protected" :> "sleeps" :> ReqBody '[JSON] [Sleep] :> Post '[JSON] [Sleep]

protected :: ConnectionPool -> Servant.Auth.Server.AuthResult User -> Server ProtectedAPI
protected pool (Servant.Auth.Server.Authenticated user) = getSleeps pool user
                                                :<|> postSleeps pool user
protected _ _ =  throwAll err401

data AuthResult val
  = BadPassword
  | NoSuchUser
  | Authenticated val
  | Indefinite

startApp :: IO ()
startApp = do
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
  doMigration migrateAll
  pool <- pgPool
  putStrLn ("starting server at port 8080" :: Text)
  run 8080 $ app pool cfg defaultCookieSettings jwtCfg

app :: ConnectionPool -> Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> Application
app pool cfg cookieSettings jwtCfg = serveWithContext api cfg (server pool cookieSettings jwtCfg)

api :: Proxy (API '[JWT])
api = Proxy

server :: ConnectionPool -> CookieSettings -> JWTSettings -> Server (API auths)
server pool cs jwts = protected pool

txt :: User -> Handler Text
txt user = return $ pack $ show user

