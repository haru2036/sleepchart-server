{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import           Protolude               hiding ( drop )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Aeson.TH                  ( defaultOptions
                                                , Options(..)
                                                , deriveJSON
                                                )
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Servant.Auth.Server
import           Control.Lens                   ( (^.)
                                                , (^?!)
                                                )
import           Crypto.JWT                     ( SignedJWT
                                                , JWTError
                                                , ClaimsSet
                                                , decodeCompact
                                                , defaultJWTValidationSettings
                                                , verifyClaims
                                                , claimSub
                                                , FromCompact
                                                , AsError
                                                , StringOrURI
                                                , string
                                                )
import           Debug.Trace
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock

type Uid = Text
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        uid Uid Unique
        name Text Maybe
        email Text Maybe
        UniqueUid uid
        deriving Show

    Sleep 
        user User
        start UTCTime
        end UTCTime
        rating Int Maybe
        deriving Show Eq
|]

instance Eq User where
    (==) x y = userUid x == userUid y

data ClientSleep = ClientSleep {csStart :: UTCTime
                               ,csEnd :: UTCTime
                               ,csRating :: Maybe Int}
                               deriving(Show, Eq)

$(deriveJSON defaultOptions  ''ClientSleep)


instance ToJWT User
instance FromJWT User where
    decodeJWT claimsSet = Right $ User (pack $ show uid) Nothing Nothing
        where uid = fromJust (claimsSet ^. claimSub) ^?! string
