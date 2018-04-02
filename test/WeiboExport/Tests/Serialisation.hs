module WeiboExport.Tests.Serialisation
  ( spec
  ) where

import API.Types

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Test.HUnit.Base
import Test.Hspec

spec :: Spec
spec =
  describe "serialisation of API datatypes" $ do
    describe "the status-list response" $ do
      before (BSL.readFile "test/sample-data/status-list-response.json") $ do
        it "can be decoded" $ \bytes -> do
          case Aeson.eitherDecode bytes of
            Left msg -> assertFailure msg
            Right StatusListResponse {} -> return ()
    describe "the status type" $ do
      before (BSL.readFile "test/sample-data/status-list-response.json") $ do
        it "can be round-triped" $ \bytes -> do
          case Aeson.eitherDecode bytes of
            Left msg -> assertFailure msg
            Right StatusListResponse {unStatusListResponse = statuses} ->
              Aeson.parseEither Aeson.parseJSON (Aeson.toJSONList statuses) `shouldBe`
              Right statuses
    describe "the comment-list response" $ do
      before (BSL.readFile "test/sample-data/comment-list-response.json") $ do
        it "can be decoded" $ \bytes -> do
          case Aeson.eitherDecode bytes of
            Left msg -> assertFailure msg
            Right CommentListResponse {} -> return ()
    describe "the comment type" $ do
      before (BSL.readFile "test/sample-data/comment-list-response.json") $ do
        it "can be round-triped" $ \bytes -> do
          case Aeson.eitherDecode bytes of
            Left msg -> assertFailure msg
            Right CommentListResponse {unCommentListResponse = comments} ->
              Aeson.parseEither Aeson.parseJSON (Aeson.toJSONList comments) `shouldBe`
              Right comments
