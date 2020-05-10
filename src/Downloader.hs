{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Downloader where

import Control.Monad.Except (catchError)

import Weibo

downloadAllPages ::
     forall m a. (MonadWeibo m)
  => (Int -> m [a])
  -> m [a]
downloadAllPages action =
  let go :: [[a]] -> Int -> Int -> m [a]
      go xss page retries = do
        xs <-
          action page `catchError` \case
            WeiboWreqError _e -> do
              return []
            WeiboParseError _e -> do
              return []
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (page + 1) retries
   in go [] 1 5
