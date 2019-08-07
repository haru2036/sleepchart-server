module Api.Sleep where
import Model
import Servant
import Servant.Auth.Server
import Database.Persist.Sql
import Database.Persist.Class
import Control.Monad.IO.Class
import DataStore.Internal
import Api.Common
import           Data.Time.Clock

getSleeps :: ConnectionPool -> User -> Handler [ClientSleep]
getSleeps pool user = 
    doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map (toClientSleep . entityVal) list

getSleepsWithRange :: ConnectionPool -> User -> Maybe UTCTime -> Maybe Int-> Handler [ClientSleep]
getSleepsWithRange pool user (Just start) (Just count) =
    doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user, SleepStart <. start] [Desc SleepStart, LimitTo count]
        return $ map (toClientSleep . entityVal) list
getSleepsWithRange _ _ _ _ = throwAll err400

postSleeps :: ConnectionPool -> User -> [ClientSleep] -> Handler [ClientSleep]
postSleeps pool user sleeps = 
    doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        _ <- putMany $ map (fromClientSleep user) sleeps
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map (toClientSleep . entityVal) list


toClientSleep :: Sleep -> ClientSleep
toClientSleep s = ClientSleep (sleepStart s) (sleepEnd s)

fromClientSleep :: User -> ClientSleep -> Sleep
fromClientSleep user cs = Sleep user (csStart cs) (csEnd cs)