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

getConfig :: SinglMsg m -> Text -> IO Config
getConfig singl txt = do
  let eFile = getDBFile singl txt
  Database{..} <- either (return . const emptyDB) getDatabase eFile
  Config <$> newIORef offset <*> newIORef userMap
  
new :: Config -> Logger.Handle -> IO Handle
new Config{..} logH = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = do
    offset <- readIORef offsetRef
    Logger.logDebug logH $ "Read IORef. Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = do
    mbN <- M.lookup userId <$> readIORef userRef
    let msg = case mbN of 
                Just x  -> unwords ["Read IORef. User", show userId, ": number of repeats -", show x]
                Nothing -> unwords ["Read IORef. User", show userId, "didn`t found in database."]
    Logger.logDebug logH msg
    return mbN
  
  setOffset :: Int -> IO ()
  setOffset n = do
    writeIORef offsetRef n
    Logger.logDebug logH $ "Write IORef. New database offset: " <> show n
  
  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    m <- readIORef userRef
    Logger.logDebug logH "Read user database from IORef."
    let newMap = M.insert userId n m
    writeIORef userRef newMap
    Logger.logDebug logH $ unwords ["Write IORef. User", show userId, ": number of repeats -", show n]

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle cfg logH f = new cfg logH >>= f