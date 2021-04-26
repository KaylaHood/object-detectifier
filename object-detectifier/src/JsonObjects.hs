{-# LANGUAGE OverloadedStrings #-}

module JsonObjects ( AnnotateImageRequestBody(..) ) where

import Data.Text (Text)
import Data.Aeson
import Control.Monad (mzero)

data AnnotateImageRequestBody =
  AnnotateImageRequestBody { location :: !Text,
                             label :: Maybe Text,
                             enable_detection :: Maybe Bool
                             } deriving Show

instance FromJSON AnnotateImageRequestBody where
  parseJSON (Object v) =
    AnnotateImageRequestBody <$> v .:  "location"
                             <*> v .:? "label"
                             <*> v .:? "enable_detection"
  parseJSON _ = mzero

instance ToJSON AnnotateImageRequestBody where
  toJSON (AnnotateImageRequestBody location label enable_detection) =
    object [ "location"         .= location,
             "label"            .= label,
             "enable_detection" .= enable_detection
            ]
