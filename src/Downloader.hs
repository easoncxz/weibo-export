{-# LANGUAGE DeriveGeneric #-}

module Downloader where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import API.Types

data DeepStatus = DeepStatus
  { statusPerSe :: Status
  , statusComments :: [Comment]
  , statusPictures :: [Picture]
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON DeepStatus

instance Aeson.FromJSON DeepStatus
