{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.IORefDB where

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
  
new :: Config -> IO Handle
new Config{..} = return $ Handle{..} where
  getOffset :: IO Int
  getOffset = readIORef offsetRef

  setOffset :: Int -> IO ()
  setOffset = writeIORef offsetRef
  
  getUserRepeatN :: UserId -> IO (Maybe Int)
  getUserRepeatN userId = M.lookup userId <$> readIORef userRef
  
  setUserRepeatN :: UserId -> Int -> IO ()
  setUserRepeatN userId n = do
    m <- readIORef userRef
    let newMap = M.insert userId n m
    writeIORef userRef newMap

withHandle :: Config -> (Handle -> IO ()) -> IO ()
withHandle cfg f = new cfg >>= f