module WeiboExport.Tests.Downloader where

import Data.Aeson
import Test.Hspec

import Downloader
import WeiboExport.Tests.SampleData

spec :: Spec
spec = do
  describe "Downloader" $ do
    describe "DeepStatus" $ do
      before
        (do s <- sampleStatusIO
            c <- sampleCommentIO
            let p = samplePictureWithoutBytes
            return (DeepStatus s [c] [p])) $ do
        it "exists" $ \ds@(DeepStatus s cs ps) -> do
          encode ds `shouldBe`
            encode
              (object
                 [ "statusPerSe" .= toJSON s
                 , "statusComments" .= toJSON cs
                 , "statusPictures" .= toJSON ps
                 ])
        it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
