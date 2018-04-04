module WeiboExport.Tests.ApiClient where

import Test.Hspec

import API.Client
import API.Types

spec :: Spec
spec = do
  describe "the picture URL" $ do
    it "renders" $ do
      largeJpgUrl (ID "abc") `shouldBe` "https://ww1.sinaimg.cn/large/abc.jpg"
