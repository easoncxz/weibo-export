module Downloader.Deep where

import Control.Lens (view)
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.String.ToString (toString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Downloader (downloadAllPages)
import Logging
import Weibo (MonadWeibo)
import qualified Weibo
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

saveDeepStatuses :: FilePath -> FilePath -> [DeepStatus] -> IO ()
saveDeepStatuses statusDir imgDir ds = do
  createDirectoryIfMissing True statusDir
  createDirectoryIfMissing True imgDir
  forM_ ds $ \deep@(DeepStatus status comments pictures) -> do
    let statusIDText =
          case status of
            StatusNormal normal -> T.unpack . getID . view #identifier $ normal
            StatusDeleted deleted ->
              T.unpack . getID . view #identifier $ deleted
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> ".json")
      (encodePretty deep)
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> "-comments.json")
      (encodePretty comments)
    forM_ pictures $ \(Picture pid bytesM) ->
      case bytesM of
        Nothing -> logError $ "Picture " ++ show pid ++ " is missing bytes"
        Just bs -> BSL.writeFile (imgDir </> toString pid ++ ".jpg") bs

downloadDeepStatus :: (MonadWeibo m) => Status -> m DeepStatus
downloadDeepStatus =
  \case
    status@(StatusNormal NormalStatus {identifier, commentsCount, picIDs}) -> do
      comments <-
        if commentsCount > 0
          then downloadAllPages
                 (\p -> view #comments <$> Weibo.getComments identifier p)
          else return []
      pictures <- sequence [Weibo.downloadPicture p | p <- picIDs]
      return DeepStatus {comments, status, pictures}
    deepStatusStatus@(StatusDeleted DeletedStatus {}) ->
      return (DeepStatus deepStatusStatus [] [])
