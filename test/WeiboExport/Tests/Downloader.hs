module WeiboExport.Tests.Downloader where

import Control.Monad
import Data.Aeson
import Test.Hspec

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
            return [DeepStatus sn [c] [p], DeepStatus sd [c] [p]]) $ do
        it "exists" $ \ss -> do
          forM_ ss $ \ds@(DeepStatus s cs ps) -> do
            encode ds `shouldBe`
              encode
                (object
                   [ "statusPerSe" .= toJSON s
                   , "statusComments" .= toJSON cs
                   , "statusPictures" .= toJSON ps
                   ])
        it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
