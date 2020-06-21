{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.IORefDB where

import qualified Logger
import           Data.Singl
import           Data.Update
import           Database.Internal
import           Data.IORef
import           Data.Text                (Text)
import qualified Data.IntMap.Strict  as M (empty, findWithDefault, lookup, insert)
import           Data.IntMap.Strict       (IntMap)

data Config = Config
  { offsetRef :: IORef Int
  , userRef   :: IORef (IntMap Int)
  }

getConfig :: SinglMsg m -> Logger.Handle -> Text -> IO Config
getConfig singl logH txt = do
  let eFile = getDBFile singl txt
  Database{..} <- case eFile of
    Left err   -> do Logger.logInfo logH "Database | Couldn`t get database file from config.ini. Initializing empty database"
                     return emptyDB
    Right file -> do Logger.logDebug logH $ "Database | Get database file from config.ini: " <> file
                     getDatabase file logH
  offsetRef <- newIORef offset
  Logger.logDebug logH "Database | Initializing offsetRef"
  userRef <- newIORef userMap
  Logger.logDebug logH "Database | Initializing userRef"
  return $ Config {..}
  
new :: Config -> Logger.Handle -> IO Handle
new Config{..} logH = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = do
    offset <- readIORef offsetRef
    Logger.logDebug logH $ "Database | Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = do
    mbN <- M.lookup userId <$> readIORef userRef
    let msg = case mbN of 
                Just x  -> unwords ["Database | Read userRef. User", show userId, ": number of repeats -", show x]
                Nothing -> unwords ["Database | Read userRef. User", show userId, "didn`t found in database."]
    Logger.logDebug logH msg
    return mbN
  
  setOffset :: Int -> IO ()
  setOffset n = do
    writeIORef offsetRef n
    Logger.logDebug logH $ "Database | Write offsetRef. New offset: " <> show n
  
  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    m <- readIORef userRef
    Logger.logDebug logH "Database | Read userRef."
    let newMap = M.insert userId n m
    writeIORef userRef newMap
    Logger.logDebug logH $ unwords ["Database | Write userRef. User", show userId, ": number of repeats -", show n]

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle cfg logH f = new cfg logH >>= f