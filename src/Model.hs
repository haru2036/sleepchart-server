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
import Servant.Auth.Server
import Control.Lens ((^.), (^?!))
import Crypto.JWT (SignedJWT, JWTError, ClaimsSet, decodeCompact, defaultJWTValidationSettings, verifyClaims, claimSub, FromCompact, AsError, StringOrURI, string)
import Debug.Trace
import Data.Maybe (fromJust)
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        name Text
        uid Text
        age Int Maybe
        deriving Show
|]
instance ToJWT User
instance FromJWT User where
    decodeJWT claimsSet = Right $ User "" (pack $ show uid) Nothing
                            where
                                uid = (fromJust $ claimsSet ^. claimSub) ^?! string
