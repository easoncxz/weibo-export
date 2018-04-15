module Downloader where

import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import GHC.Generics (Generic)
import Servant.Client (ServantError)

import API.Client
import API.Types hiding (comments)

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

downloadEverything ::
     (MonadError ServantError m, MonadReader WeiboApiClient m, MonadIO m)
  => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages getStatuses
  sequence (downloadDeepStatus <$> ss)
