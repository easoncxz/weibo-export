module Main
  ( main
  ) where

import Test.Hspec
import qualified WeiboExport.Tests.Serialisation as Serialisation

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests = describe "all tests" $ Serialisation.spec
