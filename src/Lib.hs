module Lib where

import qualified Data.Text.IO as T

import qualified API.Client as API

getClient :: IO API.WeiboApiClient
getClient = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  API.newWeiboApiClient (API.Cookie cookie)
