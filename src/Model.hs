{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text
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
        deriving Show

    Sleep json
        user User
        start UTCTime
        end UTCTime
        deriving Show Eq
|]


instance Eq User where
    (==) x y = (userUid x) == (userUid y)

instance ToJWT User
instance FromJWT User where
    decodeJWT claimsSet = Right $ User (pack $ show uid) Nothing Nothing
                            where
                                uid = (fromJust $ claimsSet ^. claimSub) ^?! string
