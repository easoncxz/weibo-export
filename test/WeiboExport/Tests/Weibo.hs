module WeiboExport.Tests.Weibo where

import qualified Data.Aeson as A
import Test.Hspec

import Weibo (UserRoot(..), findContainerID)

spec :: Spec
spec =
  describe "Weibo client" $
  it "can pick out a containerid" $ do
    Just (v :: A.Value) <-
      A.decodeFileStrict' "test/sample-data/user-top-level.json"
    findContainerID (UserRoot v) `shouldBe` Right "1076032113725781"
