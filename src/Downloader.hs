module Downloader where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics (Generic)
import Servant.Client (ServantError)

import API.Client
import API.Types

data DeepStatus = DeepStatus
  { deepStatusStatus :: Status
  , deepStatusComments :: [Comment]
  , deepStatusPictures :: [Picture]
  } deriving (Eq, Show, Generic)

instance ToJSON DeepStatus where
  toJSON DeepStatus {..} =
    object
      [ "status" .= deepStatusStatus
      , "comments" .= deepStatusComments
      , "pictures" .= deepStatusPictures
      ]

instance FromJSON DeepStatus where
  parseJSON =
    withObject "DeepStatus" $ \o ->
      DeepStatus <$> (o .: "status") <*> (o .: "comments") <*> (o .: "pictures")

downloadAllPages :: MonadError e m => (Maybe Int -> m [a]) -> m [a]
downloadAllPages action =
  let go xss n = do
        xs <- action (Just n) `catchError` \_e -> return []
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (n + 1)
   in go [] 1

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
    deepStatusStatus@(TagDeletedStatus _) ->
      return (DeepStatus deepStatusStatus [] [])

getDeepStatuses ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => Maybe Int
  -> m [DeepStatus]
getDeepStatuses mbPage = do
  ss :: [Status] <- getStatuses mbPage
  sequence [downloadDeepStatus s | s <- ss]

downloadEverything ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages getStatuses
  sequence (downloadDeepStatus <$> ss)
