{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.FileDB where

import           Data.Singl
import           Types
import           Data.Aeson
import           Data.Text                  (Text)
import qualified Data.IntMap.Strict   as M  (lookup, insert)
import qualified Data.ByteString.Lazy as BS (writeFile)
import           Database.Internal          

data Config = Config
  { dbFile :: FilePath
  } deriving Show

getConfig :: SinglMsg m -> Text -> Config
getConfig singl txt = Config $ either (error) id (getDBFile singl txt)

new :: Config -> IO Handle
new Config{..} = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = offset <$> getDatabase dbFile
  
  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = (M.lookup userId . userMap) <$> getDatabase dbFile
  
  setOffset :: Int -> IO ()
  setOffset n = do
    db <- getDatabase dbFile
    BS.writeFile dbFile $ encode db {offset = n}
  
  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    db <- getDatabase dbFile
    let newMap = M.insert userId n $ userMap db
    BS.writeFile dbFile $ encode db {userMap = newMap}
  
withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle cfg f = new cfg >>= f
  
