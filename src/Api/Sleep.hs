module Api.Sleep where
import Model
import Servant
import Database.Persist.Sql
import DataStore.Internal

getSleeps :: ConnectionPool -> User -> Handler [Sleep]
getSleeps pool user = undefined
    
    

postSleeps :: ConnectionPool -> User -> [Sleep] -> Handler [Sleep]
postSleeps = undefined