module Logging
  ( logInfo
  ) where

import Control.Monad.IO.Class
import System.IO

logInfo :: (MonadIO m, Show a) => m a -> m a
logInfo v = do
  liftIO . hPutStrLn stderr . show =<< v
  v
