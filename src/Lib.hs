module Lib where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Downloader
import Logging (logInfoM)
import qualified Storer
import Weibo (MonadWeibo, PageNum)
import qualified Weibo
import Weibo.Serialisation (Status, UserID)

-- | Bad, since the `liftIO` doesn't happen until the whole `statuses` has finished
downloadAndSaveStatuses :: MonadWeibo m => UserID -> FilePath -> m ()
downloadAndSaveStatuses uid dir = do
  cid <- Weibo.getUserContainerID uid
  statuses <- Downloader.downloadAllStatusesFromPage cid uid 1
  liftIO $ do
    goodPaths <- mapM (logInfoM . Storer.saveStatus dir) statuses
    forM_ goodPaths $ \p -> putStrLn ("Saved Status to: " <> p)
    putStrLn "All done."

reportAndSave :: MonadWeibo m => FilePath -> PageNum -> [Status] -> m [Status]
reportAndSave dir page statuses =
  liftIO $ do
    goodPaths <- mapM (Storer.saveStatus dir) statuses
    putStrLn ("Saved page " <> show page <> " of statuses")
    forM_ goodPaths $ \p -> putStrLn ("Saved Status to: " <> p)
    return statuses

downloadAndSaveStatusesSimultaneously ::
     MonadWeibo m => FilePath -> Bool -> UserID -> PageNum -> m ()
downloadAndSaveStatusesSimultaneously dir noWait uid startPage = do
  cid <- Weibo.getUserContainerID uid
  statuses <-
    Downloader.downloadAllStatusesFromPage'
      noWait
      (reportAndSave dir)
      cid
      uid
      startPage
  liftIO $
    putStrLn ("All done. Total statuses downloaded: " <> show (length statuses))
