module Downloader where

import Control.Monad.Except (catchError)
import Data.Aeson (Value)

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

downloadAllStatusesFromPage :: (MonadWeibo m) => Int -> m (Value, [Status])
downloadAllStatusesFromPage page = do
  getStatuses page >>= \case
    StatusListUnrecogniable weird -> return (weird, [])
    StatusListNormal statuses -> do
      (weird, otherStatuses) <- downloadAllStatusesFromPage (page + 1)
      return (weird, statuses ++ otherStatuses)
