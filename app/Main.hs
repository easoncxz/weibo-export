module Main where

-- import qualified Data.Text as T
import System.Environment
import System.Exit

-- import Weibo (makeWeiboApiClient, runWeiboM)
main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      putStrLn "Pass cookie as first argument"
      exitWith (ExitFailure 130)
    [_] -> do
      putStrLn "Pass weibo cointainerID as second argument"
      exitWith (ExitFailure 130)
    [_, _] -> do
      putStrLn "Pass weibo status output directory as third argument"
      exitWith (ExitFailure 130)
    [_, _, _] -> do
      putStrLn "Pass image output directory as fourth argument"
      exitWith (ExitFailure 130)
    (_cookie:_cointainerID:_statusDir:_imgDir:_) -> do
      putStrLn "Under maintenance"
      exitWith (ExitFailure 1)
