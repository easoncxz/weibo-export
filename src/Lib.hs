module Lib
  ( someFunc
  , DeepTweet(..)
  ) where

import qualified Data.ByteString.Lazy as BSL
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

_debugRun :: IO (Either Servant.ServantError [API.Status])
_debugRun = do
  c <- getClient
  API.getStatuses c Nothing

_debugComments :: Text -> IO (Either Servant.ServantError [API.Comment])
_debugComments statusID = do
  c <- getClient
  API.getComments c (API.ID statusID) Nothing

_debugDownloadPhoto :: Text -> IO ()
_debugDownloadPhoto pidT = do
  c <- getClient
  API.Picture {pictureBytes = bytes} <- API.downloadPicture c (API.ID pidT)
  BSL.writeFile "image.jpg" bytes

data DeepTweet =
  DeepTweet API.Status
            [API.Comment]
            [API.Picture]
