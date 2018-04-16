module WeiboExport.Tests.SampleData where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

import API.Types

sampleFilesIO :: FromJSON a => [FilePath] -> IO [a]
sampleFilesIO files = do
  result <-
    fmap sequence . sequence . fmap (fmap eitherDecode . BSL.readFile) $ files
  case result of
    Left msg -> fail msg
    Right responses -> return responses

sampleStatusListResponseListIO :: IO [StatusListResponse]
sampleStatusListResponseListIO =
  sampleFilesIO
    [ "test/sample-data/status-list-response.json"
    , "test/sample-data/status-list-response-with-unauthorised.json"
    ]

sampleCommentListResponseListIO :: IO [CommentListResponse]
sampleCommentListResponseListIO = do
  sampleFilesIO ["test/sample-data/comment-list-response.json"]

sampleStatusesIO :: Prism' Status b -> IO [b]
sampleStatusesIO f =
  concatMap (toListOf (statuses . each . f)) <$> sampleStatusListResponseListIO

sampleUsersIO :: IO [User]
sampleUsersIO = map (view user) <$> sampleStatusesIO _TagNormalStatus

sampleCommentListIO :: IO [Comment]
sampleCommentListIO =
  concat . fmap (view comments) <$> sampleCommentListResponseListIO

samplePictureWithoutBytes :: Picture
samplePictureWithoutBytes = Picture (ID "abc") Nothing
