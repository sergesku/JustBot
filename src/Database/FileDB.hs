{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.FileDB where

import qualified Logger as Logger
import           Data.Singl
import           Data.Update
import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.IntMap.Strict   as M  (lookup, insert)
import qualified Data.ByteString.Lazy as BS (writeFile)
import           Database.Internal          

data Config = Config
  { dbFile :: FilePath
  } deriving Show

getConfig :: SinglMsg m -> Text -> Config
getConfig singl txt = Config $ either error id $ getDBFile singl txt

new :: Config -> Logger.Handle -> IO Handle
new Config{..} logH = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = do
    Database{..} <- readDatabase
    Logger.logDebug logH $ "Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = do
    Database{..} <- readDatabase
    let mbN = M.lookup userId userMap
        msg = case mbN of 
                Just x  -> unwords ["User", show userId, ": number of repeats - ", show x]
                Nothing -> unwords ["User", show userId, "didn`t found in database."]
    Logger.logDebug logH msg
    return mbN

  setOffset :: Int -> IO ()
  setOffset n = do
    db <- readDatabase
    Logger.logDebug logH $ "New database offset: " <> show n
    writeDatabase $ db {offset = n}

  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    db <- readDatabase
    let newMap = M.insert userId n $ userMap db
    Logger.logDebug logH $ unwords ["User ", show userId, ": new number of repeats - ", show n]
    writeDatabase $ db {userMap = newMap}
  
  readDatabase :: IO Database
  readDatabase = do
    db <- getDatabase dbFile
    Logger.logDebug logH $ "Read database from file " <> dbFile
    return db
  
  writeDatabase :: Database -> IO ()
  writeDatabase db = do
    BS.writeFile dbFile $ encode db
    Logger.logDebug logH $ "Write database to file " <> dbFile

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle cfg logH f = new cfg logH >>= f
  
