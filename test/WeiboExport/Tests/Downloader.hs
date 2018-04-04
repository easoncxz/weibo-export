module WeiboExport.Tests.Downloader where

import Data.Aeson
import Test.Hspec

import Downloader
import WeiboExport.Tests.SampleData

spec :: Spec
spec = do
  describe "Downloader" $ do
    describe "status" $ do
      before
        (do s <- sampleStatusIO
            c <- sampleCommentIO
            let p = samplePictureWithoutBytes
            return (Status s [c] [p])) $ do
        it "exists" $ \status@(Status s cs ps) -> do
          encode status `shouldBe`
            encode
              (object
                 [ "statusPerSe" .= toJSON s
                 , "statusComments" .= toJSON cs
                 , "statusPictures" .= toJSON ps
                 ])
        it "round-trips" $ \status ->
          decode (encode status) `shouldBe` Just status
