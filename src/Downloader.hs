{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Downloader where

import Control.Monad
import Control.Monad.Except (catchError)
import Servant.Client (ClientError(DecodeFailure))

import Weibo

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
            WeiboParseError _e -> do
              return []
        if null xs
          then return (concat (reverse xss))
          else go (xs : xss) (page + 1) retries
   in go [] 1 5
