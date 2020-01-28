module Api.Sleep where
import           Model
import           Servant
import           Servant.Auth.Server
import           Database.Persist.Sql
import           Database.Persist.Class
import           Control.Monad.IO.Class
import           DataStore.Internal
import           Data.Time.Clock
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans
import           Api.Common
import           Api.Types

getSleeps :: RegisteredHandler [ClientSleep]
getSleeps = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map toClientSleep list

getSleepsWithRange
    :: Maybe UTCTime
    -> Maybe Int
    -> RegisteredHandler [ClientSleep]
getSleepsWithRange (Just start) (Just count) = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        list <- selectList [SleepUser ==. user, SleepStart <. start]
                           [Desc SleepStart, LimitTo count]
        return $ map toClientSleep list
getSleepsWithRange  _ _ = throwAll err400

postSleeps ::  [ClientSleep] -> RegisteredHandler [ClientSleep]
postSleeps sleeps = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $ do
        _    <- putMany $ map (fromClientSleep user) sleeps
        list <- selectList [SleepUser ==. user] [Desc SleepStart]
        return $ map toClientSleep list

putSleep :: ClientSleep -> RegisteredHandler ClientSleep
putSleep cs = do
    (pool, user) <- ask
    lift $ doIfRegistered pool user $ return $ liftIO $ flip runSqlPool pool $
        case csId cs of
            Just cskey -> do
                Just sleep <- get cskey
                if sleepUser sleep == user then
                    replace cskey (fromClientSleep user cs) >> return cs
                else
                    undefined
                    
            Nothing -> insert (fromClientSleep user cs) >>= \key -> return $ cs {csId = Just key}




toClientSleep :: Entity Sleep -> ClientSleep
toClientSleep s = ClientSleep (Just key) (sleepStart sleep) (sleepEnd sleep) (sleepRating sleep)
                    where sleep = entityVal s
                          key = entityKey s

fromClientSleep :: User -> ClientSleep -> Sleep
fromClientSleep user cs = Sleep user (csStart cs) (csEnd cs) (csRating cs)
