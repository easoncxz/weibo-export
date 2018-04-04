module WeiboExport.Tests.SampleData where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

import API.Types

sampleStatusListResponseIO :: IO StatusListResponse
sampleStatusListResponseIO = do
  bytes <- BSL.readFile "test/sample-data/status-list-response.json"
  case eitherDecode bytes of
    Left msg -> fail msg
    Right response -> return response

sampleStatusIO :: IO (Status, Status)
sampleStatusIO = do
  StatusListResponse {statusListResponseStatuses = ss} <-
    sampleStatusListResponseIO
  let normal = head [s | s@NormalStatus {} <- ss]
      deleted =
        head
          [ s
          | NormalStatus {normalStatusRetweetedStatus = Just s@DeletedStatus {}} <-
              ss
          ]
  return (normal, deleted)

sampleUserIO :: IO User
sampleUserIO = do
  (NormalStatus {normalStatusUser}, _) <- sampleStatusIO
  return normalStatusUser

sampleCommentListResponseIO :: IO CommentListResponse
sampleCommentListResponseIO = do
  bytes <- BSL.readFile "test/sample-data/comment-list-response.json"
  case eitherDecode bytes of
    Left msg -> fail msg
    Right response -> return response

sampleCommentIO :: IO Comment
sampleCommentIO =
  head . commentListResponseComments <$> sampleCommentListResponseIO

samplePictureWithoutBytes :: Picture
samplePictureWithoutBytes = Picture (ID "abc") Nothing
