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

type WeiboApi
   = Header "Accept-Encoding" Text :> Header "Cookie" Cookie :> Header "Referer" Text :> Header "User-Agent" Text :> (StatusesApi :<|> CommentsApi)

weiboApi :: Proxy WeiboApi
weiboApi = Proxy

newClientEnv :: IO ClientEnv
newClientEnv = do
  settings <- newManager tlsManagerSettings
  return $ mkClientEnv settings (BaseUrl Https "m.weibo.cn" 443 "")

localClientEnv :: IO ClientEnv
localClientEnv = do
  settings <- newManager tlsManagerSettings
  return $ mkClientEnv settings (BaseUrl Http "localhost" 5001 "")

data WeiboApiClient = WeiboApiClient
  { getStatuses :: forall m. MonadIO m =>
                               Maybe Int -> m [Status]
  , getComments :: forall m. MonadIO m =>
                               StatusID -> Maybe Int -> m [Comment]
  }

logError :: MonadIO m => String -> m ()
logError = liftIO . putStrLn

chromeUA :: Text
chromeUA =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.162 Safari/537.36"

newWeiboApiClient :: Cookie -> IO WeiboApiClient
newWeiboApiClient cookie = do
  clientEnv <- newClientEnv
  let getStatusesM :<|> getCommentsM =
        client
          weiboApi
          (Just "gzip, deflate, br")
          (Just cookie)
          (Just "https://m.weibo.cn")
          (Just chromeUA)
      handleErr :: MonadIO m => Either ServantError a -> m a
      handleErr =
        \case
          Left servantErr -> do
            case servantErr of
              ConnectionError m -> do
                logError (T.unpack m)
                fail "Failed with ConnectionError"
              UnsupportedContentType _ r -> do
                logError (BSL8.unpack $ responseBody r)
                fail "Failed with UnsupportedContentType"
              DecodeFailure t _ -> do
                logError (T.unpack t)
                fail "Failed with DecodeFailure"
              _ -> do
                logError (show servantErr)
                fail "Failed with some other ServantError"
          Right v -> return v
      getStatuses :: MonadIO m => Maybe Int -> m [Status]
      getStatuses mbPage =
        liftIO $
        fmap unStatusListResponse $
        handleErr =<< runClientM (getStatusesM (Just "cards") mbPage) clientEnv
      getComments :: MonadIO m => StatusID -> Maybe Int -> m [Comment]
      getComments statusID mbPage =
        liftIO $
        fmap unCommentListResponse $
        handleErr =<< runClientM (getCommentsM (Just statusID) mbPage) clientEnv
  return WeiboApiClient {..}

-- debugRun :: IO ()
debugRun = do
  putStr "Please provide a cookie: "
  cookie <- T.getLine
  c <- newWeiboApiClient (Cookie cookie)
  getStatuses c Nothing
