module WeiboExport.Tests.ApiClient where

import Test.Hspec

import API.Client
import API.Types

spec :: Spec
spec =
  describe "the picture URL" $
  it "renders" $
  largeJpgUrl (ID "abc") `shouldBe` "https://ww1.sinaimg.cn/large/abc.jpg"
