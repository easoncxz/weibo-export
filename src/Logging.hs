module Logging
  ( logError
  ) where

import Control.Monad.IO.Class

logError :: MonadIO m => String -> m ()
logError = liftIO . putStrLn
