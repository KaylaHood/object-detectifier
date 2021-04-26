{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApplicationController
  ( app,
   isGetImagesByTagsReq,
   isGetAllImagesReq,
   isGetImagesByIdReq,
   isPostImageAndAnnotateRequest ) where

import ImageRepository

import JsonObjects

import Control.Lens ((.~), (<&>))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception

import Network.Wai
import Network.Wai.Conduit
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method
import Network.HTTP.Types

import qualified Network.Google as Google
import qualified Network.Google.Vision as GVision

import Control.Lens

import System.IO

import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.UTF8 as BLS
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Split
import Data.Aeson
import Data.Aeson.Parser (json)
import Data.Conduit
import Data.Conduit.Attoparsec

import Debug.Trace ( trace )

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app req respond
  | isEmptyPath req = trace (show req) $ respond $ responseLBS status200 [(hContentType, "text/plain")] "Hello, world!"
  | isGetImagesByTagsReq req =
      do
        (status, response) <- doGetImagesByTags req
        trace (show req) $ respond $ responseLBS status [(hContentType, "text/plain")] (BLU.fromString (show response))
  | isGetImagesByIdReq req =
      do
        (status, response) <- doGetImageById req
        trace (show req) $ respond $ responseLBS status [(hContentType, "text/plain")] (BLU.fromString (show response))
  | isGetAllImagesReq req =
      do
        (status, response) <- doGetAllImages
        trace (show req) $ respond $ responseLBS status [(hContentType, "text/plain")] (BLU.fromString (show response))
  | isPostImageAndAnnotateRequest req =
      do
        (status, response) <- doPostImageAndAnnotate req
        trace (show req) $ respond $ responseLBS status [(hContentType, "text/plain")] (BLU.fromString (show response))
  | otherwise = trace (show req) $ respond $ responseLBS status404 [(hContentType, "text/plain")] "Request was not recognized"

{-- Tests for requests that don't provide a path (i.e. "localhost:3000/") --}
isEmptyPath :: Request -> Bool
isEmptyPath req = case rawPathInfo req of "" -> True
                                          _ -> False

{-- Example: GET /images?objects="dog,cat" --}
doGetImagesByTags :: Request -> IO (Status, [Image])
doGetImagesByTags req = do
  let query = maybe "" BLS.toString (snd (head (queryString req)))
  response <- getImagesByTags (splitOn "," query)
  -- TODO: Handle errors (don't just return 200s all the time)
  return (status200, response)

isGetImagesByTagsReq :: Request -> Bool
isGetImagesByTagsReq req
  | requestMethod req /= methodGet                                                     = False
  | head (pathInfo req) /= "images"                                                    = False
  | Prelude.length (queryString req) == 1 && fst (head (queryString req)) == "objects" = True
  | otherwise                                                                          = False

{-- Example: GET /images/{imageId} --}
doGetImageById :: Request -> IO (Status, Image)
doGetImageById req = do
  let query = T.unpack (last (pathInfo req))
  response <- getImageById query
  -- TODO: Handle errors (don't just return 200s all the time)
  return (status200, response)

isGetImagesByIdReq :: Request -> Bool
isGetImagesByIdReq req
  | requestMethod req /= methodGet                                      = False
  | head (pathInfo req) /= "images"                                     = False
  | Prelude.length (pathInfo req) /= 2                                  = False
  | T.foldr (\c acc -> C.isDigit c && acc) True (last (pathInfo req))   = True
  | otherwise                                                           = False

{-- Example: GET /images --}
doGetAllImages :: IO (Status, [(Image, ImageMetadata)])
doGetAllImages = do
  response <- getAllImageMetadata
  -- TODO: Handle errors (don't just return 200s all the time)
  return (status200, response)

isGetAllImagesReq :: Request -> Bool
isGetAllImagesReq req
  | requestMethod req == methodGet && pathInfo req == ["images"] = True
  | otherwise                                                    = False

{-- Example With File: POST /images {"location": "C:\Users\kayla\My Pictures\Dog.jpg", "label": "Dog", "enable_detection": true} --}
{-- Example With URL:  POST /images {"location": "https://i.natgeofe.com/n/4f5aaece-3300-41a4-b2a8-ed2708a0a27c/domestic-dog_thumb.jpg", "label": "Dog", "enable_detection": true} --}
doPostImageAndAnnotate :: Request -> IO (Status, String)
doPostImageAndAnnotate req = do
  annotateRequestJson <- runConduit $ sourceRequestBody req .| sinkParser json
  case fromJSON annotateRequestJson of
    Success (AnnotateImageRequestBody location _ _) -> do
      maybeHandle <- handle (\(e :: IOException) -> print e >> return Nothing) $ do
        h <- openFile (T.unpack location) ReadMode
        return (Just h)
      case maybeHandle of
        Just h -> do
          contents <- trace (show h) $ Base64URL.encode <$> B.hGetContents h
          hClose h
          lgr <- Google.newLogger Google.Trace stderr
          mgr <- newManager tlsManagerSettings
          crd <- Google.getApplicationDefault mgr
          env <- Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ GVision.cloudVisionScope)
          let imageReq = set GVision.gcvviContent (Just contents) GVision.googleCloudVisionV1p2beta1Image
              annotateImageReq = [set GVision.gcvvairFeatures
                [set GVision.gcvvfType (Just GVision.ObjectLocalization) GVision.googleCloudVisionV1p2beta1Feature]
                (set GVision.gcvvairImage
                  (Just imageReq)
                  GVision.googleCloudVisionV1p2beta1AnnotateImageRequest)]
              googleReq = set GVision.gcvvbairRequests annotateImageReq GVision.googleCloudVisionV1p2beta1BatchAnnotateImagesRequest
          response <- runResourceT . Google.runGoogle env . Google.send $ GVision.imagesAnnotate googleReq
          -- TODO: Handle errors from Google API
          return (status200, show response)
        _ -> do
          lgr <- Google.newLogger Google.Trace stderr
          mgr <- newManager tlsManagerSettings
          crd <- Google.getApplicationDefault mgr
          env <- Google.newEnvWith crd lgr mgr <&> (Google.envScopes .~ GVision.cloudVisionScope)
          let imageReq = set GVision.gcvviSource
                          (Just (set GVision.gcvvisImageURI
                            (Just location)
                            GVision.googleCloudVisionV1p2beta1ImageSource))
                          GVision.googleCloudVisionV1p2beta1Image
              annotateImageReq = [set GVision.gcvvairFeatures
                [set GVision.gcvvfType (Just GVision.ObjectLocalization) GVision.googleCloudVisionV1p2beta1Feature]
                (set GVision.gcvvairImage
                  (Just imageReq)
                  GVision.googleCloudVisionV1p2beta1AnnotateImageRequest)]
              googleReq = set GVision.gcvvbairRequests
                          annotateImageReq
                          GVision.googleCloudVisionV1p2beta1BatchAnnotateImagesRequest
          response <- runResourceT . Google.runGoogle env . Google.send $ GVision.imagesAnnotate googleReq
          -- TODO: Handle errors from Google API
          return (status200, show response)
    _ -> do return (status400, "There was an error parsing the request body")

isPostImageAndAnnotateRequest :: Request -> Bool
isPostImageAndAnnotateRequest req
  | requestMethod req == methodPost && pathInfo req == ["images"] = True
  | otherwise                                                     = False

