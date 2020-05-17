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
  , getUserContainerID
  , getComments
  , getStatuses
  , downloadPicture
  --
  , PageNum
  ) where

import Control.Lens
import Control.Monad.Except (ExceptT(..), MonadError, liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Reader (ask)
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.URI as HTTP
import qualified Network.Wreq as Wreq

import Weibo.Serialisation

data WeiboError
  = WeiboWreqError (Wreq.Response BSL.ByteString)
  | WeiboParseError String
  deriving (Show, Generic)

type PageNum = Int

-- | Unit of mocking
data WeiboApiClient m =
  WeiboApiClient
    { weiboApiGetUserRoot :: UserID -> m UserRoot
    , weiboApiGetStatuses :: ContainerID -> UserID -> PageNum -> m StatusListResponse
    , weiboApiGetComments :: StatusID -> PageNum -> m CommentListResponse
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
weiboUrl slash = "https://m.weibo.cn/api" <> slash

-- | https://m.weibo.cn/api/container/getIndex\?type\=uid\&value\=$uid
getUserRootImpl :: UserID -> IO (Either WeiboError UserRoot)
getUserRootImpl (ID uid) = do
  resp <-
    Wreq.getWith
      (foldl
         (&)
         Wreq.defaults
         [ Wreq.param "type" .~ ["uid"]
         , Wreq.param "value" .~ [T.pack (show uid)]
         ])
      (weiboUrl "/container/getIndex")
  return . Bi.bimap WeiboParseError UserRoot $
    Aeson.eitherDecode (resp ^. Wreq.responseBody)

-- | Low-level
getStatusesImpl ::
     ContainerID
  -> UserID
  -> PageNum
  -> IO (Either WeiboError StatusListResponse)
getStatusesImpl (ID containerid) (ID uid) page = do
  let opts =
        foldl
          (&)
          Wreq.defaults
          [ Wreq.param "type" .~ ["uid"]
          , Wreq.param "value" .~ [T.pack (show uid)]
          , Wreq.param "containerid" .~ [containerid]
          , Wreq.param "page" .~ [T.pack (show page)]
          ]
  resp <- Wreq.getWith opts (weiboUrl "/container/getIndex")
  case resp ^. Wreq.responseStatus . Wreq.statusCode of
    200 ->
      let ei =
            Aeson.eitherDecode (resp ^. Wreq.responseBody) :: Either String StatusListResponse
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

makeWeiboApiClient :: IO (WeiboApiClient WeiboM)
makeWeiboApiClient =
  return $
  WeiboApiClient
    { weiboApiGetUserRoot = \u -> liftWeibo (getUserRootImpl u)
    , weiboApiGetStatuses = \c u p -> liftWeibo (getStatusesImpl c u p)
    , weiboApiGetComments =
        \_ _ -> liftWeibo $ return (Right (CommentListResponse [])) -- upstream API broken
    , weiboApiDownloadPicture = liftWeibo . downloadPictureImpl
    }

largeJpgUrl :: PictureID -> String
largeJpgUrl (ID pid) =
  BS8.toString $ "https://ww1.sinaimg.cn/large/" <>
  HTTP.urlEncode True (T.encodeUtf8 pid) <>
  ".jpg"

getUserContainerID :: MonadWeibo m => UserID -> m ContainerID
getUserContainerID uid = do
  WeiboApiClient {weiboApiGetUserRoot} <- ask
  root <- weiboApiGetUserRoot uid
  liftEither (Bi.bimap WeiboParseError id (parseContainerID root))

getComments :: MonadWeibo m => StatusID -> PageNum -> m CommentListResponse
getComments statusID page = do
  WeiboApiClient {weiboApiGetComments} <- ask
  weiboApiGetComments statusID page

getStatuses ::
     MonadWeibo m => ContainerID -> UserID -> PageNum -> m StatusListResponse
getStatuses cid uid page = do
  WeiboApiClient {weiboApiGetStatuses} <- ask
  weiboApiGetStatuses cid uid page

downloadPicture :: MonadWeibo m => PictureID -> m Picture
downloadPicture pictureID = do
  WeiboApiClient {weiboApiDownloadPicture} <- ask
  weiboApiDownloadPicture pictureID
