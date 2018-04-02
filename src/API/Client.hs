{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module API.Client
  ( WeiboApiClient(..)
  , newWeiboApiClient
  , Cookie(..)
  ) where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BS8
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.URI as HTTP
import qualified Network.Wreq as Wreq
import Servant.API
import Servant.Client

import API.Types

newtype Cookie = Cookie
  { unCookie :: Text
  } deriving (FromHttpApiData, ToHttpApiData, Show)

type CommentsApi
   = "api" :> "comments" :> "show" :> QueryParam "id" StatusID :> QueryParam "page" Int :> Get '[ JSON] CommentListResponse

type StatusesApi
   = "index" :> "my" :> QueryParam "format" Text :> QueryParam "page" Int :> Get '[ JSON] StatusListResponse

type WeiboApi = Header "Cookie" Cookie :> (StatusesApi :<|> CommentsApi)

weiboApi :: Proxy WeiboApi
weiboApi = Proxy

largeJpgUrl :: PictureID -> String
largeJpgUrl (ID pid) =
  BS8.toString $
  "https://ww1.sinaimg.cn/large/" <> HTTP.urlEncode True (T.encodeUtf8 pid) <>
  ".jpg"

data WeiboApiClient = WeiboApiClient
  { getStatuses :: forall m. MonadIO m =>
                               Maybe Int -> m (Either ServantError [Status])
  , getComments :: forall m. MonadIO m =>
                               StatusID -> Maybe Int -> m (Either ServantError [Comment])
  , downloadPicture :: forall m. MonadIO m =>
                                   PictureID -> m Picture
  }

newWeiboApiClient :: Cookie -> IO WeiboApiClient
newWeiboApiClient cookie = do
  let newClientEnv :: IO ClientEnv
      newClientEnv = do
        settings <- newManager tlsManagerSettings
        return $ mkClientEnv settings (BaseUrl Https "m.weibo.cn" 443 "")
  clientEnv <- newClientEnv
  let getStatusesM :<|> getCommentsM = client weiboApi (Just cookie)
      getStatuses mbPage = do
        fmap (fmap statusListResponseStatuses) . liftIO $
          runClientM (getStatusesM (Just "cards") mbPage) clientEnv
      getComments statusID mbPage =
        fmap (fmap commentListResponseComments) . liftIO $
        runClientM (getCommentsM (Just statusID) mbPage) clientEnv
      downloadPicture pid =
        liftIO $ do
          r <- Wreq.get (largeJpgUrl pid)
          let pictureID = pid
              pictureBytes = r ^. Wreq.responseBody
          return Picture {..}
  return WeiboApiClient {..}
