module Main where

import qualified Data.Text.IO as T
import Servant.Client (ServantError)

import API.Client
import Downloader

main :: IO (Either ServantError [DeepStatus])
main = do
  cookieT <- T.getLine
  client <- newWeiboApiClient (Cookie cookieT)
  runWeiboClientM downloadEverything client
