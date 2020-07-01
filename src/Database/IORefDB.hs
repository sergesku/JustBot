{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.IORefDB where

import           Logger                     (AppMonad, logDebug, logInfo, logWarning, logError)
import           Data.Singl
import           Data.Update
import           Database.Internal
import           Data.IORef
import           Control.Monad.IO.Class
import           Data.Text                (Text)
import qualified Data.IntMap.Strict  as M (empty, findWithDefault, lookup, insert)
import           Data.IntMap.Strict       (IntMap)

data Config = Config
  { offsetRef :: IORef Int
  , userRef   :: IORef (IntMap Int)
  }

getConfig :: SinglMsg m -> Text -> AppMonad Config
getConfig singl txt = do
  let eFile = getDBFile singl txt
  Database{..} <- case eFile of
    Left err   -> do logInfo "Database | Couldn`t get database file from config.ini. Initializing empty database"
                     return emptyDB
    Right file -> do logDebug $ "Database | Get database file from config.ini: " <> file
                     initializeDatabase file
  offsetRef <- liftIO $ newIORef offset
  logDebug "Database | Initializing offsetRef"
  userRef <- liftIO $ newIORef userMap
  logDebug "Database | Initializing userRef"
  return $ Config {..}
  
new :: Config -> AppMonad Handle
new Config{..} = return $ Handle{..} where
  getOffset :: AppMonad Int
  getOffset = do
    offset <- liftIO $ readIORef offsetRef
    logDebug $ "Database | Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> AppMonad (Maybe Int)
  getUserRepeatN userId = do
    mbN <- liftIO $ M.lookup userId <$> readIORef userRef
    let msg = case mbN of 
                Just x  -> unwords ["Database | Read userRef. User", show userId, ": number of repeats -", show x]
                Nothing -> unwords ["Database | Read userRef. User", show userId, "didn`t found in database."]
    logDebug msg
    return mbN
  
  setOffset :: Int -> AppMonad ()
  setOffset n = do
    liftIO $ writeIORef offsetRef n
    logDebug $ "Database | Write offsetRef. New offset: " <> show n
  
  setUserRepeatN :: UserId -> Int -> AppMonad ()
  setUserRepeatN userId n = do
    m <- liftIO $ readIORef userRef
    logDebug "Database | Read userRef."
    let newMap = M.insert userId n m
    liftIO $ writeIORef userRef newMap
    logDebug $ unwords ["Database | Write userRef. User", show userId, ": number of repeats -", show n]

withHandle :: Config -> (Handle -> AppMonad ()) -> AppMonad ()
withHandle cfg f = new cfg >>= f