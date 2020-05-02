{-# LANGUAGE TypeOperators #-}

module API.Client
  ( Cookie(..)
  , WeiboApiClient
  , WeiboApiM
  , downloadPicture
  , getComments
  , getStatuses
  , largeJpgUrl
  , newWeiboApiClient
  , runWeiboClientM
  ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import Data.Proxy
import qualified Data.Sequence as Seq
import Data.Text
import qualified Data.Text.Encoding as T

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.URI as HTTP
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Lens as Wreq
import Servant.API
  ( (:<|>)(..)
  , (:>)
  , FromHttpApiData
  , Get
  , Header
  , JSON
  , QueryParam
  , ToHttpApiData
  )
import qualified Servant.Client as Servant

import API.Types
import Logging

newtype Cookie =
  Cookie
    { unCookie :: Text
    }
  deriving (FromHttpApiData, ToHttpApiData, Show)

type CommentsApi
   = "api" :> "comments" :> "show" :> QueryParam "id" StatusID :> QueryParam "page" Int :> Get '[ JSON] CommentListResponse

type StatusesApi
   = "index" :> "my" :> QueryParam "format" Text :> QueryParam "page" Int :> Get '[ JSON] StatusListResponse

type WeiboApi = Header "Cookie" Cookie :> (StatusesApi :<|> CommentsApi)

weiboApi :: Proxy WeiboApi
weiboApi = Proxy

data WeiboApiClient =
  WeiboApiClient
    { weiboApiGetStatuses :: Maybe Int -> IO (Either Servant.ClientError StatusListResponse)
    , weiboApiGetComments :: StatusID -> Maybe Int -> IO (Either Servant.ClientError CommentListResponse)
    }

newWeiboApiClient :: Cookie -> IO WeiboApiClient
newWeiboApiClient cookie = do
  settings <- newManager tlsManagerSettings
  let clientEnv =
        Servant.mkClientEnv
          settings
          (Servant.BaseUrl Servant.Https "m.weibo.cn" 443 "")
      getStatusesM :<|> getCommentsM = Servant.client weiboApi (Just cookie)
      weiboApiGetStatuses mbPage =
        Servant.runClientM (getStatusesM (Just "cards") mbPage) clientEnv
      weiboApiGetComments statusID mbPage =
        Servant.runClientM (getCommentsM (Just statusID) mbPage) clientEnv
  return WeiboApiClient {..}

type WeiboApiM = ExceptT Servant.ClientError (ReaderT WeiboApiClient IO)

getStatuses ::
     (MonadError Servant.ClientError m, MonadIO m, MonadReader WeiboApiClient m)
  => Maybe Int
  -> m [Status]
getStatuses mbPage = do
  c <- ask
  fmap (view statuses) . liftEither =<<
    liftIO (logInfoM (weiboApiGetStatuses c mbPage))

getComments ::
     (MonadError Servant.ClientError m, MonadIO m, MonadReader WeiboApiClient m)
  => StatusID
  -> Maybe Int
  -> m [Comment]
getComments statusID mbPage = do
  c <- ask
  fmap (view comments) . liftEither =<<
    liftIO (logInfoM (weiboApiGetComments c statusID mbPage))

runWeiboClientM ::
     WeiboApiM a -> WeiboApiClient -> IO (Either Servant.ClientError a)
runWeiboClientM = runReaderT . runExceptT

largeJpgUrl :: PictureID -> String
largeJpgUrl (ID pid) =
  BS8.toString $ "https://ww1.sinaimg.cn/large/" <>
  HTTP.urlEncode True (T.encodeUtf8 pid) <>
  ".jpg"

servantResponseFromWreq :: Wreq.Response BSL.ByteString -> Servant.Response
servantResponseFromWreq wr =
  Servant.Response
    { responseStatusCode = wr ^. Wreq.responseStatus
    , responseHeaders = Seq.fromList (wr ^. Wreq.responseHeaders)
    , responseHttpVersion = wr ^. Wreq.responseVersion
    , responseBody = wr ^. Wreq.responseBody
    }

downloadPicture ::
     (MonadIO m, MonadError Servant.ClientError m) => PictureID -> m Picture
downloadPicture pid = do
  wr <- liftIO $ Wreq.get (largeJpgUrl pid)
  when (wr ^. Wreq.responseStatus . Wreq.statusCode /= 200) $ throwError .
    Servant.FailureResponse undefined .
    servantResponseFromWreq $
    wr
  let _pictureIdentifier = pid
      _pictureBytes = Just (wr ^. Wreq.responseBody)
  logInfoM $ return Picture {..}
