module Api.Sleep where
import           Model
import           Servant
import           Servant.Auth.Server
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           DataStore.Internal
import           Api.Common
import           Data.Time.Clock
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans

getSleeps :: ReaderT (ConnectionPool, User) Handler [ClientSleep]
getSleeps = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map (toClientSleep . entityVal) list

getSleepsWithRange
    :: Maybe UTCTime
    -> Maybe Int
    -> ReaderT (ConnectionPool, User) Handler [ClientSleep]
getSleepsWithRange (Just start) (Just count) = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user, SleepStart <. start]
                           [Desc SleepStart, LimitTo count]
        return $ map (toClientSleep . entityVal) list
getSleepsWithRange  _ _ = throwAll err400

postSleeps ::  [ClientSleep] -> ReaderT (ConnectionPool, User) Handler [ClientSleep]
postSleeps sleeps = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        _    <- putMany $ map (fromClientSleep user) sleeps
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map (toClientSleep . entityVal) list


toClientSleep :: Sleep -> ClientSleep
toClientSleep s = ClientSleep (sleepStart s) (sleepEnd s) (sleepRating s)

fromClientSleep :: User -> ClientSleep -> Sleep
fromClientSleep user cs = Sleep user (csStart cs) (csEnd cs) (csRating cs)
