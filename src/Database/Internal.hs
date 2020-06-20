{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Internal where

import Data.Singl
import Data.Ini.Config
import Types
import Data.Aeson
import Data.IntMap.Strict (IntMap, empty)
import Data.Text                  (Text)

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

getDatabase :: FilePath -> IO Database
getDatabase file = maybe emptyDB id <$> decodeFileStrict file

emptyDB :: Database
emptyDB = Database 0 empty