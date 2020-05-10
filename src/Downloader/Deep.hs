module Downloader.Deep where

import Data.Aeson
import GHC.Generics (Generic)

import Downloader (downloadAllPages)
import Weibo (MonadWeibo, downloadPicture, getComments)
import Weibo.Serialisation

data DeepStatus =
  DeepStatus
    { status :: Status
    , comments :: [Comment]
    , pictures :: [Picture]
    }
  deriving (Eq, Show, Generic)

instance ToJSON DeepStatus where
  toJSON DeepStatus {status, comments, pictures} =
    object ["status" .= status, "comments" .= comments, "pictures" .= pictures]

instance FromJSON DeepStatus where
  parseJSON =
    withObject "DeepStatus" $ \o ->
      DeepStatus <$> (o .: "status") <*> (o .: "comments") <*> (o .: "pictures")

downloadDeepStatus :: (MonadWeibo m) => Status -> m DeepStatus
downloadDeepStatus =
  \case
    status@(StatusNormal NormalStatus {identifier, commentsCount, picIDs}) -> do
      comments <-
        if commentsCount > 0
          then downloadAllPages (getComments identifier)
          else return []
      pictures <- sequence [downloadPicture p | p <- picIDs]
      return DeepStatus {comments, status, pictures}
    deepStatusStatus@(StatusDeleted DeletedStatus {}) ->
      return (DeepStatus deepStatusStatus [] [])
