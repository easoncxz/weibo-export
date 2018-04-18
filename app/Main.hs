module Main where

import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as BSL
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
      putStrLn "Pass output filename as second argument"
      exitWith (ExitFailure 1)
    (cookie:outfile:_) -> do
      client <- newWeiboApiClient (Cookie (T.pack cookie))
      runWeiboClientM downloadEverything client >>= \case
        Left e -> do
          putStrLn "Failed with some ServantError after some retries"
          print e
          exitWith (ExitFailure 13)
        Right ds -> BSL.writeFile outfile (A.encodePretty ds)
