module WeiboExport.Tests.Weibo where

import qualified Data.Aeson as A
import Test.Hspec

import Weibo.Serialisation (ID(ID), UserRoot(..), parseContainerID)

spec :: Spec
spec =
  describe "Weibo client" $
  it "can pick out a containerid" $ do
    Just (v :: A.Value) <-
      A.decodeFileStrict' "test/sample-data/user-top-level.json"
    parseContainerID (UserRoot v) `shouldBe` Right (ID "1076032113725781")
