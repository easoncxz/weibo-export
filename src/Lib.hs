module Lib where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Downloader
import Logging (logInfoM)
import qualified Storer
import Weibo (MonadWeibo)
import Weibo.Serialisation (Status)

-- | Bad, since the `liftIO` doesn't happen until the whole `statuses` has finished
downloadAndSaveStatuses :: MonadWeibo m => FilePath -> m ()
downloadAndSaveStatuses dir = do
  statuses <- Downloader.downloadAllStatusesFromPage 1
  liftIO $ do
    goodPaths <- mapM (logInfoM . Storer.saveStatus dir) statuses
    forM_ goodPaths $ \p -> putStrLn ("Saved Status to: " <> p)
    putStrLn "All done."

reportAndSave :: MonadWeibo m => FilePath -> Int -> [Status] -> m [Status]
reportAndSave dir page statuses =
  liftIO $ do
    goodPaths <- mapM (Storer.saveStatus dir) statuses
    putStrLn ("Saved page " <> show page <> " of statuses")
    forM_ goodPaths $ \p -> putStrLn ("Saved Status to: " <> p)
    return statuses

downloadAndSaveStatusesSimultaneously ::
     MonadWeibo m => FilePath -> Bool -> Int -> m ()
downloadAndSaveStatusesSimultaneously dir noWait startPage = do
  statuses <-
    Downloader.downloadAllStatusesFromPage' noWait (reportAndSave dir) startPage
  liftIO $
    putStrLn ("All done. Total statuses downloaded: " <> show (length statuses))
