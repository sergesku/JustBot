{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Internal where

import           Control.Exception
import           Data.Error
import qualified Logger
import           Data.Singl
import           Data.Ini.Config
import           Data.Update
import           Data.Aeson
import           Data.Maybe         ( fromMaybe )
import           Data.IntMap.Strict ( IntMap, empty )
import           Data.Text                  ( Text )

data Handle = Handle
  { getOffset      :: IO Int
  , setOffset      :: Int -> IO ()
  , getUserRepeatN :: UserId -> IO (Maybe Int)
  , setUserRepeatN :: UserId -> Int -> IO ()
  }

data ConfigDB = ConfigDB
  { dbFile :: FilePath
  } deriving Show

data Database = Database
  { offset  :: Int
  , userMap :: IntMap Int
  } deriving Show

instance FromJSON Database where
  parseJSON = withObject "Database" $ \ o -> Database
    <$> o .: "offset"
    <*> o .: "userMap"

instance ToJSON Database where
  toJSON Database{..} = object
    [ "offset"  .= offset
    , "userMap" .= userMap
    ]

getDBFile :: SinglMsg m -> Text -> Either String FilePath
getDBFile singl txt = parseIniFile txt (dbFileParser singl)

dbFileParser :: SinglMsg m -> IniParser FilePath
dbFileParser m = case m of
                  STG -> parser "TG" "tg.db"
                  SVK -> parser "VK" "vk.db"
  where parser sName def = section sName $ fieldDefOf "database" string def

initializeDatabase :: FilePath -> Logger.Handle -> IO Database
initializeDatabase file logH = do
  mbDB <- decodeFileStrict file
  case mbDB of
    Just _  -> Logger.logDebug logH $ "Database | Read database from file " <> file
    Nothing -> Logger.logInfo logH $ "Database | Couldn`t read database from file " <> file <> ". Initializing empty database"
  return $ fromMaybe emptyDB mbDB

emptyDB :: Database
emptyDB = Database 0 empty