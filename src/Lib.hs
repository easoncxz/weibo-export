module Lib where

import qualified Data.Text.IO as T

import qualified Weibo

getClient :: IO (Weibo.WeiboApiClient Weibo.WeiboM)
getClient = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  Weibo.newWeiboApiClient (Weibo.Cookie cookie)
