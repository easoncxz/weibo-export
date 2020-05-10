module Lib where

import qualified Data.Text.IO as T

import qualified Weibo

getClient :: IO (Weibo.WeiboApiClient Weibo.WeiboM)
getClient = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  putStr "Please provide a cointainerID: "
  cointainerID <- T.getLine
  return $ Weibo.makeWeiboApiClient cookie cointainerID
