{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Downloader where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import API.Client
import API.Types

data DeepStatus = DeepStatus
  { statusPerSe :: Status
  , statusComments :: [Comment]
  , statusPictures :: [Picture]
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON DeepStatus

instance Aeson.FromJSON DeepStatus

downloadPage ::
     (MonadReader WeiboApiClient m, MonadIO m) => Int -> m [DeepStatus]
downloadPage _page = do
  _client <- ask
  undefined

download :: (MonadReader WeiboApiClient m, MonadIO m) => m [DeepStatus]
download = fmap concat (mapM downloadPage [1 ..])
