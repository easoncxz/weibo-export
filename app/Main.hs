module Main where

import qualified Data.Text as T
import System.Environment
import System.Exit

import Downloader
import Weibo

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      putStrLn "Pass cookie as first argument"
      exitWith (ExitFailure 1)
    [_] -> do
      putStrLn "Pass weibo cointainerID as second argument"
      exitWith (ExitFailure 1)
    [_, _] -> do
      putStrLn "Pass weibo status output directory as third argument"
      exitWith (ExitFailure 1)
    [_, _, _] -> do
      putStrLn "Pass image output directory as fourth argument"
      exitWith (ExitFailure 1)
    (cookie:cointainerID:statusDir:imgDir:_) -> do
      client <- newWeiboApiClient (Cookie (T.pack cookie)) (T.pack cointainerID)
      runWeiboM client downloadEverything >>= \case
        Left e -> do
          putStrLn "Failed with some ServantError after some retries"
          print e
          exitWith (ExitFailure 13)
        Right ds -> saveDeepStatuses statusDir imgDir ds
