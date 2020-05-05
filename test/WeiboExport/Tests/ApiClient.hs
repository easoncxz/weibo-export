module WeiboExport.Tests.ApiClient where

import Test.Hspec

import Weibo (largeJpgUrl)
import Weibo.Serialisation (ID(ID))

spec :: Spec
spec =
  describe "the picture URL" $
  it "renders" $
  largeJpgUrl (ID "abc") `shouldBe` "https://ww1.sinaimg.cn/large/abc.jpg"
