{-# LANGUAGE DeriveGeneric #-}

module Downloader where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Servant.Client (ServantError)

import API.Client
import API.Types

data DeepStatus = DeepStatus
  { deepStatusStatus :: Status
  , deepStatusComments :: [Comment]
  , deepStatusPictures :: [Picture]
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON DeepStatus

instance Aeson.FromJSON DeepStatus

downloadAllPages :: Applicative m => (Maybe Int -> m [a]) -> m [a]
downloadAllPages action = concat <$> traverse (action . Just) [1 ..]

downloadDeepStatus ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => Status
  -> m DeepStatus
downloadDeepStatus =
  \case
    deepStatusStatus@(TagNormalStatus normalStatus) -> do
      deepStatusComments <-
        downloadAllPages (getComments (normalStatus ^. identifier))
      deepStatusPictures <-
        sequence [downloadPicture p | p <- normalStatus ^. picIDs]
      return DeepStatus {..}
    deepStatusStatus@(TagDeletedStatus _) -> do
      return (DeepStatus deepStatusStatus [] [])
