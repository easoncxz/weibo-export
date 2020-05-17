module Main
  ( main
  ) where

import Test.Hspec
import qualified WeiboExport.Tests.ApiClient as ApiClient
import qualified WeiboExport.Tests.Downloader as Downloader
import qualified WeiboExport.Tests.Serialisation as Serialisation
import qualified WeiboExport.Tests.Weibo as Weibo

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests =
  describe "all tests" $ do
    Weibo.spec
    Serialisation.spec
    ApiClient.spec
    Downloader.spec
