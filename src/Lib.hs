module Lib where

import Control.Monad
import qualified Data.Text as T

import qualified Downloader
import qualified Storer
import Weibo (MonadWeibo)

downloadAndSaveStatuses :: MonadWeibo m => FilePath -> m (IO ())
downloadAndSaveStatuses dir = do
  (weird, statuses) <- Downloader.downloadAllStatusesFromPage 1
  return $ do
    let tag = T.pack (show (length statuses))
    weirdPath <- Storer.saveUnrecognisableStatusListResponse dir tag weird
    putStrLn ("Saved StatusListUnrecogniable to: " <> weirdPath)
    goodPaths <- mapM (Storer.saveStatus dir) statuses
    forM_ goodPaths $ \p -> putStrLn ("Saved Status to: " <> p)
