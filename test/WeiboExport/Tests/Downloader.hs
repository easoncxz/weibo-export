module WeiboExport.Tests.Downloader where

import Control.Monad
import Data.Aeson
import Test.Hspec

import API.Types
import Downloader
import WeiboExport.Tests.SampleData

spec :: Spec
spec = do
  describe "Downloader" $ do
    describe "DeepStatus" $ do
      before
        (do (sn, sd) <- sampleStatusIO
            c <- sampleCommentIO
            let p = samplePictureWithoutBytes
            return
              [ DeepStatus (TagNormalStatus sn) [c] [p]
              , DeepStatus (TagDeletedStatus sd) [c] [p]
              ]) $ do
        it "exists" $ \ss -> do
          forM_ ss $ \ds@(DeepStatus s cs ps) -> do
            encode ds `shouldBe`
              encode
                (object
                   [ "status" .= toJSON s
                   , "comments" .= toJSON cs
                   , "pictures" .= toJSON ps
                   ])
        it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
