module Downloader where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except (catchError)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.String.ToString
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.Client (ClientError(DecodeFailure))
import System.Directory
import System.FilePath.Posix

import Logging
import Weibo
import Weibo.Serialisation

data DeepStatus =
  DeepStatus
    { deepStatusStatus :: Status
    , deepStatusComments :: [Comment]
    , deepStatusPictures :: [Picture]
    }
  deriving (Eq, Show, Generic)

instance ToJSON DeepStatus where
  toJSON DeepStatus {..} =
    object
      [ "status" .= deepStatusStatus
      , "comments" .= deepStatusComments
      , "pictures" .= deepStatusPictures
      ]

instance FromJSON DeepStatus where
  parseJSON =
    withObject "DeepStatus" $ \o ->
      DeepStatus <$> (o .: "status") <*> (o .: "comments") <*> (o .: "pictures")

downloadAllPages ::
     forall m a. (MonadWeibo m)
  => (Maybe Int -> m [a])
  -> m [a]
downloadAllPages action =
  let go :: [[a]] -> Int -> Int -> m [a]
      go xss page retries = do
        xs <-
          action (Just page) `catchError` \case
            WeiboServantError (DecodeFailure _ _) -> return []
            WeiboServantError _other -> do
              if retries == 0
                then return []
                else do
                  go xss page (retries - 1)
            WeiboWreqError _e -> do
              return []
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (page + 1) retries
   in go [] 1 5

downloadDeepStatus :: (MonadWeibo m) => Status -> m DeepStatus
downloadDeepStatus status =
  case status of
    deepStatusStatus@(TagNormalStatus normalStatus) -> do
      deepStatusComments <-
        if normalStatus ^. commentsCount > 0
          then downloadAllPages (getComments (normalStatus ^. identifier))
          else return []
      deepStatusPictures <-
        sequence [downloadPicture p | p <- normalStatus ^. picIDs]
      return DeepStatus {..}
    deepStatusStatus@(TagDeletedStatus _) ->
      return (DeepStatus deepStatusStatus [] [])

downloadEverything :: MonadWeibo m => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages getStatuses
  sequence (downloadDeepStatus <$> ss)

saveDeepStatuses :: FilePath -> FilePath -> [DeepStatus] -> IO ()
saveDeepStatuses statusDir imgDir ds = do
  createDirectoryIfMissing True statusDir
  createDirectoryIfMissing True imgDir
  forM_ ds $ \d@(DeepStatus s cs ps) -> do
    let statusIDMaybe =
          s ^? _TagNormalStatus . identifier <|> s ^? _TagDeletedStatus .
          identifier
        statusIDText = statusIDMaybe & fromJust & getID & T.unpack
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> ".json")
      (encodePretty d)
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> "-comments.json")
      (encodePretty cs)
    forM_ ps $ \(Picture pid bytesM) ->
      case bytesM of
        Nothing -> logError $ "Picture " ++ show pid ++ " is missing bytes"
        Just bs -> BSL.writeFile (imgDir </> toString pid ++ ".jpg") bs
