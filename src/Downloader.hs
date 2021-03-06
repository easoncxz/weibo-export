module Downloader where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)

import Weibo
import Weibo.Serialisation

retry :: MonadWeibo m => Int -> m a -> m (Maybe a)
retry times action =
  if times <= 0
    then return Nothing
    else (Just <$> action) `catchError` \_ -> retry (times - 1) action

downloadAllPages ::
     forall m a. (MonadWeibo m)
  => (Int -> m [a])
  -> m [a]
downloadAllPages action = doPage 1
  where
    doPage p = do
      got <- retry 5 (action p)
      case got of
        Nothing -> return []
        Just several -> do
          rest <- doPage (p + 1)
          return (several ++ rest)

downloadAllStatusesFromPage ::
     (MonadWeibo m) => ContainerID -> UserID -> PageNum -> m [Status]
downloadAllStatusesFromPage = downloadAllStatusesFromPage' False (const return)

-- | Provide a hook to take a look
downloadAllStatusesFromPage' ::
     (MonadWeibo m)
  => Bool
  -> (Int -> [Status] -> m [Status])
  -> ContainerID
  -> UserID
  -> PageNum
  -> m [Status]
downloadAllStatusesFromPage' noWait look cid userID page = do
  liftIO $
    putStrLn ("Starting download for page " <> show page <> " of statuses...")
  getStatuses cid userID page >>= \case
    StatusListUnrecogniable _weird -> return []
    StatusListNormal statuses -> do
      saw <- look page statuses
      if mod page 5 == 0 && not noWait
        then liftIO $ threadDelay (5 * 1000 * 1000)
        else return ()
      otherStatuses <-
        downloadAllStatusesFromPage' noWait look cid userID (page + 1)
      return (saw ++ otherStatuses)
