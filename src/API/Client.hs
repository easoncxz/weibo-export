{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module API.Client where

import API.Types
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

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

data WeiboApiClient = WeiboApiClient
  { getStatuses :: forall m. MonadIO m =>
                               Maybe Int -> m (Either ServantError [Status])
  , getComments :: forall m. MonadIO m =>
                               StatusID -> Maybe Int -> m (Either ServantError [Comment])
  }

logError :: MonadIO m => String -> m ()
logError = liftIO . putStrLn

newWeiboApiClient :: Cookie -> IO WeiboApiClient
newWeiboApiClient cookie = do
  let newClientEnv :: IO ClientEnv
      newClientEnv = do
        settings <- newManager tlsManagerSettings
        return $ mkClientEnv settings (BaseUrl Https "m.weibo.cn" 443 "")
  clientEnv <- newClientEnv
  let getStatusesM :<|> getCommentsM = client weiboApi (Just cookie)
      getStatuses mbPage = do
        fmap (fmap unStatusListResponse) . liftIO $
          runClientM (getStatusesM (Just "cards") mbPage) clientEnv
      getComments statusID mbPage =
        fmap (fmap unCommentListResponse) . liftIO $
        runClientM (getCommentsM (Just statusID) mbPage) clientEnv
  return WeiboApiClient {..}

-- debugRun :: IO ()
debugRun = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  c <- newWeiboApiClient (Cookie cookie)
  getStatuses c Nothing
