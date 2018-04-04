module WeiboExport.Tests.Serialisation
  ( spec
  ) where

import API.Types

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Test.Hspec

import WeiboExport.Tests.SampleData

spec :: Spec
spec =
  describe "serialisation of API datatypes" $ do
    describe "the status-list response" $ do
      it "can be decoded" $ void sampleStatusListResponseIO
    describe "the status type" $ do
      before sampleStatusIO $ do
        it "can be round-triped" $ \status -> do
          parseEither parseJSON (toJSON status) `shouldBe` Right status
    describe "the comment-list response" $ do
      it "can be decoded" $ void sampleCommentListResponseIO
    describe "the comment type" $ do
      before sampleCommentIO $ do
        it "can be round-triped" $ \comment ->
          parseEither parseJSON (toJSON comment) `shouldBe` Right comment
    describe "the picture type" $ do
      it "can be round-tripped only if there are no bytes" $ do
        let pid = ID "abc"
            p = Picture {pictureID = pid, pictureBytes = Nothing}
            pb = Picture {pictureID = pid, pictureBytes = Just "123"}
        parseEither parseJSON (toJSON p) `shouldBe` Right p
        parseEither parseJSON (toJSON pb) `shouldBe` Right p
