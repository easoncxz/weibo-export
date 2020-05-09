{-# LANGUAGE TypeApplications #-}

module WeiboExport.Tests.Downloader where

import Data.Aeson
import Data.Generics.Sum (_Ctor)
import Test.Hspec

import Downloader
import Weibo.Serialisation
import WeiboExport.Tests.SampleData

spec :: Spec
spec =
  describe "Downloader" $
  describe "DeepStatus" $
  before
    (do ns <- sampleStatusesIO (_Ctor @"StatusNormal")
        ds <- sampleStatusesIO (_Ctor @"StatusDeleted")
        cs <- sampleCommentListIO
        let p = samplePictureWithoutBytes
        return $
          [DeepStatus (StatusNormal n) cs [p] | n <- ns] ++
          [DeepStatus (StatusDeleted d) cs [p] | d <- ds]) $ do
    it "uses certain keys" $
      mapM_ $ \ds@(DeepStatus s cs ps) ->
        encode ds `shouldBe`
        encode (object ["status" .= s, "comments" .= cs, "pictures" .= ps])
    it "round-trips" $ \ds -> decode (encode ds) `shouldBe` Just ds
