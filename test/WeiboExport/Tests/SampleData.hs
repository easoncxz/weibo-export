module WeiboExport.Tests.SampleData where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

import API.Types

sampleStatusListResponseIO :: IO StatusListResponse
sampleStatusListResponseIO = do
  bs <- BSL.readFile "test/sample-data/status-list-response.json"
  case eitherDecode bs of
    Left msg -> fail msg
    Right response -> return response

sampleStatusIO :: IO (NormalStatus, DeletedStatus)
sampleStatusIO = do
  StatusListResponse {_statusListResponseStatuses = ss} <-
    sampleStatusListResponseIO
  let normal = head [s | TagNormalStatus s@NormalStatus {} <- ss]
      deleted =
        head
          [ s
          | TagNormalStatus NormalStatus {_normalStatusRetweetedStatus = Just (TagDeletedStatus s@DeletedStatus {})} <-
              ss
          ]
  return (normal, deleted)

sampleUserIO :: IO User
sampleUserIO = do
  (NormalStatus {_normalStatusUser}, _) <- sampleStatusIO
  return _normalStatusUser

sampleCommentListResponseIO :: IO CommentListResponse
sampleCommentListResponseIO = do
  bs <- BSL.readFile "test/sample-data/comment-list-response.json"
  case eitherDecode bs of
    Left msg -> fail msg
    Right response -> return response

sampleCommentIO :: IO Comment
sampleCommentIO =
  head . _commentListResponseComments <$> sampleCommentListResponseIO

samplePictureWithoutBytes :: Picture
samplePictureWithoutBytes = Picture (ID "abc") Nothing
