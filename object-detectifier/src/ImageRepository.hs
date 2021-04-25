module ImageRepository where

import Database.HDBC
import Database.HDBC.Sqlite3
import Image
import Data.Maybe

getImagesByTag :: String -> IO([Image])
getImagesByTag s = do
  conn <- connectSqlite3 "images.db"
  results <- quickQuery' conn "SELECT IMG.ID, IMG.NAME, IMG.SRC FROM IMAGE_TAG TAG LEFT JOIN IMAGE IMG ON IMG.ID = TAG.IMG_ID WHERE TAG_NAME=?" [toSql s]
  disconnect conn
  return (catMaybes (map fromSqlValue results))


