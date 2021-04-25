{-# LANGUAGE OverloadedStrings #-}

module ApplicationController
    ( app
    ) where

import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Google
import Debug.Trace (trace)

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app req f =
  trace (show req) f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

-- GET /images?objects="dog,cat"
-- doGetImagesByTag :: String -> IO String

-- GET /images/{imageId}
-- doGetImageById :: String -> IO String

-- GET /images
-- doGetAllImages :: String -> IO String

-- POST /images
-- doPostImageAndAnnotate :: String -> IO String

