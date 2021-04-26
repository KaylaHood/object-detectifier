{-# LANGUAGE OverloadedStrings #-}

module Main where

import ApplicationController (app)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app
  
--main :: IO ()
--main = do
--  images <- getImagesByTag "dog"
--  putStrLn $ show images
