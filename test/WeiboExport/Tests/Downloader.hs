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
        (do ns <- sampleStatusesIO _TagNormalStatus
            ds <- sampleStatusesIO _TagDeletedStatus
            cs <- sampleCommentListIO
            let p = samplePictureWithoutBytes
            return $
              [DeepStatus (TagNormalStatus n) cs [p] | n <- ns] ++
              [DeepStatus (TagDeletedStatus d) cs [p] | d <- ds]) $ do
        it "exists" $
          mapM_ $ \ds@(DeepStatus s cs ps) -> do
            encode ds `shouldBe`
              encode
                (object ["status" .= s, "comments" .= cs, "pictures" .= ps])
        it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
