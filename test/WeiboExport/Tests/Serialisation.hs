module WeiboExport.Tests.Serialisation
  ( spec
  ) where

import Weibo.Serialisation

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Test.Hspec

import WeiboExport.Tests.SampleData

spec :: Spec
spec =
  describe "serialisation of API datatypes" $ do
    describe "StatusListResponse" $
      it "can be decoded" $ void sampleStatusListResponseListIO
    describe "Status" $ do
      describe "NormalStatus" $
        before (sampleStatusesIO _TagNormalStatus) $
        it "can be round-triped" $
        mapM_ $ \normal -> do
          parseEither parseJSON (toJSON normal) `shouldBe` Right normal
          parseEither parseJSON (toJSON (TagNormalStatus normal)) `shouldBe`
            Right (TagNormalStatus normal)
      describe "DeletedStatus" $
        before (sampleStatusesIO _TagDeletedStatus) $
        it "can be round-tripped" $
        mapM_ $ \deleted -> do
          parseEither parseJSON (toJSON deleted) `shouldBe` Right deleted
          parseEither parseJSON (toJSON (TagDeletedStatus deleted)) `shouldBe`
            Right (TagDeletedStatus deleted)
    describe "CommentListResponse" $
      it "can be decoded" $ void sampleCommentListResponseListIO
    describe "Comment" $
      before sampleCommentListIO $
      it "can be round-triped" $
      mapM_
        (\comment ->
           parseEither parseJSON (toJSON comment) `shouldBe` Right comment)
    describe "Picture" $
      it "can be round-tripped only if there are no bytes" $ do
        let pid = ID "abc"
            p = Picture {_pictureIdentifier = pid, _pictureBytes = Nothing}
            pb = Picture {_pictureIdentifier = pid, _pictureBytes = Just "123"}
        parseEither parseJSON (toJSON p) `shouldBe` Right p
        parseEither parseJSON (toJSON pb) `shouldBe` Right p
    describe "User" $
      before sampleUsersIO $
      it "can be round-tripped through JSON" $
      mapM_ $ \u -> parseEither parseJSON (toJSON u) `shouldBe` Right u
