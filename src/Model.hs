{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where

import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        name Text
        uid Text
        age Int Maybe
        deriving Show
|]