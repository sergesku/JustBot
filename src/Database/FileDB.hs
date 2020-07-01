{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.FileDB where

import           Logger                     (AppMonad, logDebug, logInfo, logWarning, logError)
import           Data.Singl
import           Data.Update
import           Data.Aeson
import           Control.Monad.IO.Class
import           Data.Text                  (Text)
import qualified Data.IntMap.Strict   as M  (lookup, insert)
import qualified Data.ByteString.Lazy as BS (writeFile)
import           Database.Internal          

data Config = Config
  { dbFile :: FilePath
  } deriving Show

getConfig :: SinglMsg m -> Text -> AppMonad Config
getConfig singl txt = do
  let eFile = getDBFile singl txt
  case eFile of
    Right dbFile -> do logDebug $ "Database | Get database file from config.ini: " <> dbFile
                       return $ Config {..}
    Left err     -> do logError "Database | Couldn`t get database file from config.ini"
                       error err

new :: Config -> AppMonad Handle
new Config{..} = return $ Handle{..} where
  getOffset :: AppMonad Int
  getOffset = do
    Database{..} <- readDatabase
    logDebug $ "Database | Current offset: " <> show offset
    return offset

  getUserRepeatN :: UserId -> AppMonad (Maybe Int)
  getUserRepeatN userId = do
    Database{..} <- readDatabase
    let mbN = M.lookup userId userMap
        msg = case mbN of 
                Just x  -> unwords ["Database | User", show userId, ": number of repeats - ", show x]
                Nothing -> unwords ["Database | User", show userId, "didn`t found in database."]
    logDebug msg
    return mbN

  setOffset :: Int -> AppMonad ()
  setOffset n = do
    db <- readDatabase
    logDebug $ "Database | Set offset: " <> show n
    writeDatabase $ db {offset = n}

  setUserRepeatN :: UserId -> Int -> AppMonad ()
  setUserRepeatN userId n = do
    db <- readDatabase
    let newMap = M.insert userId n $ userMap db
    logDebug $ unwords ["Database | User ", show userId, ": new number of repeats - ", show n]
    writeDatabase $ db {userMap = newMap}
  
  readDatabase :: AppMonad Database
  readDatabase = initializeDatabase dbFile
  
  writeDatabase :: Database -> AppMonad ()
  writeDatabase db = do
    liftIO $ BS.writeFile dbFile $ encode db
    logDebug $ "Database | Write to file " <> dbFile

withHandle :: Config -> (Handle -> AppMonad ()) -> AppMonad ()
withHandle cfg f = new cfg >>= f
  
