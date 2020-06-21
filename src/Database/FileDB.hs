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

getConfig :: SinglMsg m -> Logger.Handle -> Text -> IO Config
getConfig singl logH txt = do
  let eFile = getDBFile singl txt
  case eFile of
    Right dbFile -> do Logger.logDebug logH $ "Database | Get database file from config.ini: " <> dbFile
                       return $ Config {..}
    Left err     -> do Logger.logError logH "Database | Couldn`t get database file from config.ini"
                       error err

new :: Config -> Logger.Handle -> IO Handle
new Config{..} logH = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = do
    Database{..} <- readDatabase
    Logger.logDebug logH $ "Database | Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = do
    Database{..} <- readDatabase
    let mbN = M.lookup userId userMap
        msg = case mbN of 
                Just x  -> unwords ["Database | User", show userId, ": number of repeats - ", show x]
                Nothing -> unwords ["Database | User", show userId, "didn`t found in database."]
    Logger.logDebug logH msg
    return mbN

  setOffset :: Int -> IO ()
  setOffset n = do
    db <- readDatabase
    Logger.logDebug logH $ "Database | Set offset: " <> show n
    writeDatabase $ db {offset = n}

  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    db <- readDatabase
    let newMap = M.insert userId n $ userMap db
    Logger.logDebug logH $ unwords ["Database | User ", show userId, ": new number of repeats - ", show n]
    writeDatabase $ db {userMap = newMap}
  
  readDatabase :: IO Database
  readDatabase = getDatabase dbFile logH
  
  writeDatabase :: Database -> IO ()
  writeDatabase db = do
    BS.writeFile dbFile $ encode db
    Logger.logDebug logH $ "Database | Write to file " <> dbFile

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle cfg logH f = new cfg logH >>= f
  
