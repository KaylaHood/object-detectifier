module Image (
  Image(..),
  fromSqlValue
) where

import Database.HDBC.SqlValue

data Image = Image {
  id :: String,
  name :: String,
  src :: String
} deriving (Show)

fromSqlValue :: [SqlValue] -> Maybe Image
fromSqlValue (x:y:z:[]) = Just (Image (fromSql x) (fromSql y) (fromSql z))
fromSqlValue _ = Nothing

