module Logging
  ( logInfo
  ) where

import Control.Monad.IO.Class
import System.IO

logInfo :: (MonadIO m, Show a) => m a -> m a
logInfo v = do
  liftIO . hPrint stderr =<< v
  v
