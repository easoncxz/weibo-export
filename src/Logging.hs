module Logging where

import Control.Monad.IO.Class
import System.IO

logInfoM :: (MonadIO m, Show a) => m a -> m a
logInfoM m = do
  v <- m
  liftIO . hPutStrLn stderr . ("[INFO] " ++) . show $ v
  return v

logError :: (MonadIO m, Show a) => a -> m ()
logError = liftIO . hPutStrLn stderr . ("[ERROR] " ++) . show
