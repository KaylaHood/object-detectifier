module ImageRepository
  ( getImagesByTags,
  getImageById,
  getAllImageMetadata,
  Image(..),
  ImageMetadata,
  imageFromSqlValue,
  imageAndMetadataFromSqlValue
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe

data Image = Image {
  id :: String,
  name :: String,
  src :: String
} deriving (Show)

newtype ImageMetadata
  = ImageMetadata {tags :: String}
  deriving Show

imageFromSqlValue :: [SqlValue] -> Maybe Image
imageFromSqlValue [x, y, z] = Just (Image (fromSql x) (fromSql y) (fromSql z))
imageFromSqlValue _ = Nothing

imageAndMetadataFromSqlValue :: [SqlValue] -> Maybe (Image, ImageMetadata)
imageAndMetadataFromSqlValue [x, y, z, q] = Just (Image (fromSql x) (fromSql y) (fromSql z), ImageMetadata (fromSql q))
imageAndMetadataFromSqlValue _ = Nothing

getImagesByTags :: [String] -> IO [Image]
getImagesByTags [] = do return []
getImagesByTags [x] = do
  conn <- connectSqlite3 "images.db"
  results <- quickQuery' conn "SELECT IMG.ID, IMG.NAME, IMG.SRC FROM IMAGE_TAG TAG LEFT JOIN IMAGE IMG ON IMG.ID = TAG.IMG_ID WHERE TAG.NAME = ?" [toSql x]
  disconnect conn
  return (mapMaybe imageFromSqlValue results)
-- This pattern has the query defined in the 'let' block instead of inline because, for unknown reasons, quickQuery' was not expanding the '?' for this version
getImagesByTags (x:xs) = do
  let tags_str = "('" ++ foldr (\z agg -> agg ++ "','" ++ z ++ "'") x xs ++ ")"
      query = "SELECT IMG.ID, IMG.NAME, IMG.SRC FROM IMAGE_TAG TAG LEFT JOIN IMAGE IMG ON IMG.ID = TAG.IMG_ID WHERE TAG.NAME IN " ++ tags_str
  conn <- connectSqlite3 "images.db"
  results <- quickQuery' conn query []
  disconnect conn
  return (mapMaybe imageFromSqlValue results)

getImageById :: String -> IO Image
getImageById i = do
  conn <- connectSqlite3 "images.db"
  results <- quickQuery' conn "SELECT ID, NAME, SRC FROM IMAGE WHERE ID = ?" [toSql i]
  disconnect conn
  return (head (mapMaybe imageFromSqlValue results))

getAllImageMetadata :: IO [(Image, ImageMetadata)]
getAllImageMetadata = do
  conn <- connectSqlite3 "images.db"
  results <- quickQuery' conn "SELECT IMG.ID as ID, MAX(IMG.NAME) as NAME, MAX(IMG.SRC) as SRC, GROUP_CONCAT(TAG.NAME, ',') as TAGS FROM IMAGE_TAG TAG LEFT JOIN IMAGE IMG ON IMG.ID = TAG.IMG_ID GROUP BY IMG.ID" []
  disconnect conn
  return (mapMaybe imageAndMetadataFromSqlValue results)


