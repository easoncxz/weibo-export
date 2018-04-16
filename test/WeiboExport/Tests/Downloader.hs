module WeiboExport.Tests.Downloader where

import Data.Aeson
import Test.Hspec

import API.Types
import Downloader
import WeiboExport.Tests.SampleData

spec :: Spec
spec =
  describe "Downloader" $
  describe "DeepStatus" $
  before
    (do ns <- sampleStatusesIO _TagNormalStatus
        ds <- sampleStatusesIO _TagDeletedStatus
        cs <- sampleCommentListIO
        let p = samplePictureWithoutBytes
        return $
          [DeepStatus (TagNormalStatus n) cs [p] | n <- ns] ++
          [DeepStatus (TagDeletedStatus d) cs [p] | d <- ds]) $ do
    it "uses certain keys" $
      mapM_ $ \ds@(DeepStatus s cs ps) ->
        encode ds `shouldBe`
        encode (object ["status" .= s, "comments" .= cs, "pictures" .= ps])
    it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
