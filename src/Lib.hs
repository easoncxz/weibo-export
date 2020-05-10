module Lib where

import Control.Lens (view)
import Control.Monad
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.String.ToString (toString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Downloader (downloadAllPages)
import Downloader.Deep (DeepStatus(..), downloadDeepStatus)
import Logging
import Weibo (MonadWeibo)
import qualified Weibo
import Weibo.Serialisation (ID(..), Picture(..), Status(..))

downloadEverything :: MonadWeibo m => m [DeepStatus]
downloadEverything = do
  ss <- downloadAllPages Weibo.getStatuses
  sequence (downloadDeepStatus <$> ss)

saveDeepStatuses :: FilePath -> FilePath -> [DeepStatus] -> IO ()
saveDeepStatuses statusDir imgDir ds = do
  createDirectoryIfMissing True statusDir
  createDirectoryIfMissing True imgDir
  forM_ ds $ \deep@(DeepStatus status comments pictures) -> do
    let statusIDText =
          case status of
            StatusNormal normal -> T.unpack . getID . view #identifier $ normal
            StatusDeleted deleted ->
              T.unpack . getID . view #identifier $ deleted
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> ".json")
      (encodePretty deep)
    BSL.writeFile
      (statusDir </> "status-" <> statusIDText <> "-comments.json")
      (encodePretty comments)
    forM_ pictures $ \(Picture pid bytesM) ->
      case bytesM of
        Nothing -> logError $ "Picture " ++ show pid ++ " is missing bytes"
        Just bs -> BSL.writeFile (imgDir </> toString pid ++ ".jpg") bs

getClient :: IO (Weibo.WeiboApiClient Weibo.WeiboM)
getClient = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  putStr "Please provide a cointainerID: "
  cointainerID <- T.getLine
  Weibo.newWeiboApiClient (Weibo.Cookie cookie) cointainerID
