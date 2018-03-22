module Main
  ( main
  ) where

import Test.Hspec
import qualified WeiboExport.Tests.Serialisation as Serialisation

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests = do
  describe "serialisation of API datatypes" Serialisation.spec
