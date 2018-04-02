module Lib
  ( someFunc
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Servant.Client as Servant

import qualified API.Client as API
import qualified API.Types as API

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getClient :: IO API.WeiboApiClient
getClient = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  API.newWeiboApiClient (API.Cookie cookie)

debugRun :: IO (Either Servant.ServantError [API.Status])
debugRun = do
  c <- getClient
  API.getStatuses c Nothing

debugComments :: Text -> IO (Either Servant.ServantError [API.Comment])
debugComments statusID = do
  c <- getClient
  API.getComments c (API.ID statusID) Nothing

data DeepTweet =
  DeepTweet API.Status
            [API.Comment]
            [API.Picture]
