module Api.Sleep where
import Model
import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import DataStore.Internal

getSleeps :: ConnectionPool -> User -> Handler [Sleep]
getSleeps pool user = liftIO $ flip runSqlPool pool $ do
    list <- selectList [SleepUser ==. user] [Desc SleepStart]
    return $ map entityVal list
    
    

postSleeps :: ConnectionPool -> User -> [Sleep] -> Handler [Sleep]
postSleeps = undefined