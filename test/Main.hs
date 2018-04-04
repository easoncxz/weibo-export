module Main
  ( main
  ) where

import Test.Hspec
import qualified WeiboExport.Tests.ApiClient as ApiClient
import qualified WeiboExport.Tests.Downloader as Downloader
import qualified WeiboExport.Tests.Serialisation as Serialisation

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests =
  describe "all tests" $ do
    Serialisation.spec
    ApiClient.spec
    Downloader.spec
