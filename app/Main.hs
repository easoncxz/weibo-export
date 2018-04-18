module Main where

import qualified Data.Text as T
import System.Environment
import System.Exit

import API.Client
import Downloader

main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
      putStrLn "Pass cookie as first argument"
      exitWith (ExitFailure 1)
    (_:[]) -> do
      putStrLn "Pass weibo status output directory as second argument"
      exitWith (ExitFailure 1)
    (_:_:[]) -> do
      putStrLn "Pass image output directory as second argument"
      exitWith (ExitFailure 1)
    (cookie:statusDir:imgDir:_) -> do
      client <- newWeiboApiClient (Cookie (T.pack cookie))
      runWeiboClientM downloadEverything client >>= \case
        Left e -> do
          putStrLn "Failed with some ServantError after some retries"
          print e
          exitWith (ExitFailure 13)
        Right ds -> saveDeepStatuses statusDir imgDir ds
