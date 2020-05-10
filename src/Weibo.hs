{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Weibo
  ( WeiboError(..)
  , WeiboApiClient(..)
  , MonadWeibo
  , WeiboM -- hiding constructor!
  , runWeiboM
  , makeWeiboApiClient
  , largeJpgUrl
  , getComments
  , getStatuses
  , downloadPicture
  ) where

import Control.Lens
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Reader (ask)
import Data.Aeson (eitherDecode)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.URI as HTTP
import qualified Network.Wreq as Wreq
import System.Exit (ExitCode(..), exitWith)

import Weibo.Serialisation

data WeiboError
  = WeiboWreqError (Wreq.Response BSL.ByteString)
  | WeiboParseError String
  deriving (Show, Generic)

-- | Unit of mocking
data WeiboApiClient m =
  WeiboApiClient
    { weiboApiGetStatuses :: Int -> m StatusListResponse
    , weiboApiGetComments :: StatusID -> Int -> m CommentListResponse
    , weiboApiDownloadPicture :: PictureID -> m Picture
    }
  deriving (Generic)

-- | Convenience for defining datatype-generic functions as application logic
type MonadWeibo m
   = (MonadReader (WeiboApiClient m) m, MonadError WeiboError m, MonadIO m)

-- | Hide MonadIO (fail!)
newtype WeiboM a =
  WeiboM
    { unWeiboM :: ReaderT (WeiboApiClient WeiboM) (ExceptT WeiboError IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (WeiboApiClient WeiboM)
           , MonadError WeiboError
           , MonadIO
           )

runWeiboM :: WeiboApiClient WeiboM -> WeiboM a -> IO (Either WeiboError a)
runWeiboM client = runExceptT . flip runReaderT client . unWeiboM

-- | Convenience and escape hatch for IO
liftWeibo :: IO (Either WeiboError a) -> WeiboM a
liftWeibo = WeiboM . ReaderT . const . ExceptT

weiboUrl :: String -> String
weiboUrl slash = "https://m.weibo.cn" <> slash

getStatusesImpl ::
     Text -> HeadersConfig -> Int -> IO (Either WeiboError StatusListResponse)
getStatusesImpl containerID (HeadersConfig headersMap) page = do
  let opts =
        foldl
          (&)
          Wreq.defaults
          [ Wreq.header (CI.mk name) .~ [val]
          | (name, val) <- Map.toList headersMap
          ] &
        Wreq.param "containerid" .~
        [containerID]
  resp <-
    Wreq.getWith
      (opts & Wreq.param "page" .~ (map (T.pack . show) [page]))
      (weiboUrl "/api/container/getIndex")
  case resp ^. Wreq.responseStatus . Wreq.statusCode of
    200 ->
      let ei =
            eitherDecode (resp ^. Wreq.responseBody) :: Either String StatusListResponse
       in return $ Bi.first WeiboParseError ei
    _ -> return (Left (WeiboWreqError resp))

downloadPictureImpl :: PictureID -> IO (Either WeiboError Picture)
downloadPictureImpl pid = do
  resp <- Wreq.get (largeJpgUrl pid)
  if (resp ^. Wreq.responseStatus . Wreq.statusCode /= 200)
    then return (Left (WeiboWreqError resp))
    else let identifier = pid
             bytes = Just (resp ^. Wreq.responseBody)
          in return (Right Picture {identifier, bytes})

makeWeiboApiClient :: Text -> FilePath -> IO (WeiboApiClient WeiboM)
makeWeiboApiClient containerID headersFilePath = do
  headerFileBytes <- BSL.readFile headersFilePath
  case eitherDecode headerFileBytes of
    Left msg -> do
      putStrLn ("Error parsing your Firefox headers file: " <> msg)
      exitWith (ExitFailure 130)
    Right (headers :: HeadersConfig) -> do
      return $
        WeiboApiClient
          { weiboApiGetStatuses =
              liftWeibo . getStatusesImpl containerID headers
          , weiboApiGetComments =
              \_ _ -> liftWeibo $ return (Right (CommentListResponse [])) -- upstream API broken
          , weiboApiDownloadPicture = liftWeibo . downloadPictureImpl
          }

largeJpgUrl :: PictureID -> String
largeJpgUrl (ID pid) =
  BS8.toString $ "https://ww1.sinaimg.cn/large/" <>
  HTTP.urlEncode True (T.encodeUtf8 pid) <>
  ".jpg"

getComments :: MonadWeibo m => StatusID -> Int -> m CommentListResponse
getComments statusID page = do
  WeiboApiClient {weiboApiGetComments} <- ask
  weiboApiGetComments statusID page

getStatuses :: MonadWeibo m => Int -> m StatusListResponse
getStatuses page = do
  WeiboApiClient {weiboApiGetStatuses} <- ask
  weiboApiGetStatuses page

downloadPicture :: MonadWeibo m => PictureID -> m Picture
downloadPicture pictureID = do
  WeiboApiClient {weiboApiDownloadPicture} <- ask
  weiboApiDownloadPicture pictureID
