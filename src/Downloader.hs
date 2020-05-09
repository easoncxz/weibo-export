{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Downloader where

import Control.Lens (view)
import Control.Monad
import Control.Monad.Except (catchError)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Labels ()
import Data.String.ToString (toString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.Client (ClientError(DecodeFailure))
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Logging
import Weibo
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

downloadAllPages ::
     forall m a. (MonadWeibo m)
  => (Maybe Int -> m [a])
  -> m [a]
downloadAllPages action =
  let go :: [[a]] -> Int -> Int -> m [a]
      go xss page retries = do
        xs <-
          action (Just page) `catchError` \case
            WeiboServantError (DecodeFailure _ _) -> return []
            WeiboServantError _other -> do
              if retries == 0
                then return []
                else do
                  go xss page (retries - 1)
            WeiboWreqError _e -> do
              return []
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (page + 1) retries
   in go [] 1 5

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

downloadEverything :: MonadWeibo m => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages getStatuses
  sequence (downloadDeepStatus <$> ss)

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
