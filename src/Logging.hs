module Logging where

import Control.Monad.IO.Class
import System.IO

logInfoM :: (MonadIO m, Show a) => m a -> m a
logInfoM m = do
  v <- m
  liftIO . hPrint stderr $ v
  return v

logError :: (MonadIO m, Show a) => a -> m ()
logError = liftIO . hPrint stderr
