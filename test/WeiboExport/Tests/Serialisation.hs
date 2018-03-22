module WeiboExport.Tests.Serialisation
  ( spec
  ) where

import API.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Test.HUnit.Base
import Test.Hspec

spec :: Spec
spec =
  describe "the status-list response" $ do
    before (BSL.readFile "test/sample-data/status-list-response.json") $ do
      it "can be decoded" $ \bytes -> do
        case Aeson.eitherDecode bytes of
          Left msg -> assertFailure msg
          Right (_ :: StatusListResponse) -> return ()
