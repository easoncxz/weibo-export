module WeiboExport.Tests.SampleData where

import Control.Lens
import Data.Generics.Labels ()
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

import Weibo.Serialisation

sampleFilesIO :: FromJSON a => [FilePath] -> IO [a]
sampleFilesIO files = do
  result <- fmap sequence . traverse (fmap eitherDecode . BSL.readFile) $ files
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
sampleCommentListResponseListIO =
  sampleFilesIO ["test/sample-data/comment-list-response.json"]

sampleStatusesIO :: Prism' Status b -> IO [b]
sampleStatusesIO f =
  concatMap (toListOf (#_statusListResponseStatuses . each . f)) <$> sampleStatusListResponseListIO

sampleUsersIO :: IO [User]
sampleUsersIO = map (view #_normalStatusUser) <$> sampleStatusesIO #_TagNormalStatus

sampleCommentListIO :: IO [Comment]
sampleCommentListIO =
  concatMap (view #_commentListResponseComments) <$> sampleCommentListResponseListIO

samplePictureWithoutBytes :: Picture
samplePictureWithoutBytes = Picture (ID "abc") Nothing
