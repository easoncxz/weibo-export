module Downloader where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics (Generic)
import Servant.Client (ServantError(DecodeFailure))

import API.Client
import API.Types
import Logging

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

downloadAllPages ::
     forall m a. (MonadError ServantError m, MonadIO m)
  => (Maybe Int -> m [a])
  -> m [a]
downloadAllPages action =
  let go :: [[a]] -> Int -> Int -> m [a]
      go xss page retries = do
        xs <-
          action (Just page) `catchError` \e ->
            case e of
              DecodeFailure _ _ -> return []
              other -> do
                logError other
                go xss page (retries - 1)
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (page + 1) retries
   in go [] 1 5

downloadDeepStatus ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => Status
  -> m DeepStatus
downloadDeepStatus =
  \case
    deepStatusStatus@(TagNormalStatus normalStatus) -> do
      deepStatusComments <-
        if normalStatus ^. commentsCount > 0
          then downloadAllPages (getComments (normalStatus ^. identifier))
          else return []
      deepStatusPictures <-
        sequence [downloadPicture p | p <- normalStatus ^. picIDs]
      return DeepStatus {..}
    deepStatusStatus@(TagDeletedStatus _) ->
      return (DeepStatus deepStatusStatus [] [])

downloadEverything ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages getStatuses
  sequence (downloadDeepStatus <$> ss)
