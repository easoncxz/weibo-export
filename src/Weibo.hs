{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Weibo
  ( Cookie(..)
  , WeiboError(..)
  , WeiboApiClient(..)
  , MonadWeibo
  , WeiboM -- hiding constructor!
  , runWeiboM
  , newWeiboApiClient
  , largeJpgUrl
  , getComments
  , getStatuses
  , downloadPicture
  ) where

import Control.Lens
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Reader (ask)
import Data.Aeson (eitherDecode)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS8
import Data.Maybe (maybeToList)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.URI as HTTP
import qualified Network.Wreq as Wreq
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

import Weibo.Serialisation

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

data WeiboError
  = WeiboServantError Servant.ClientError
  | WeiboWreqError (Wreq.Response BSL.ByteString)
  | WeiboParseError String
  deriving (Show, Generic)

-- | Unit of mocking
data WeiboApiClient m =
  WeiboApiClient
    { weiboApiGetStatuses :: Maybe Int -> m [Status]
    , weiboApiGetComments :: StatusID -> Maybe Int -> m [Comment]
    , weiboApiDownloadPicture :: PictureID -> m Picture
    }

-- | Convenience for defining datatype-generic functions as application logic
type MonadWeibo m = (MonadReader (WeiboApiClient m) m, MonadError WeiboError m)

-- | Hide MonadIO
newtype WeiboM a =
  WeiboM
    { unWeiboM :: ReaderT (WeiboApiClient WeiboM) (ExceptT WeiboError IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (WeiboApiClient WeiboM)
           , MonadError WeiboError
           )

runWeiboM :: WeiboApiClient WeiboM -> WeiboM a -> IO (Either WeiboError a)
runWeiboM client = runExceptT . flip runReaderT client . unWeiboM

-- | Convenience and escape hatch for IO
liftWeibo :: IO (Either WeiboError a) -> WeiboM a
liftWeibo = WeiboM . ReaderT . const . ExceptT

weiboUrl :: String -> String
weiboUrl slash = "https://m.weibo.cn" <> slash

myGetStatus :: Text -> Text -> Maybe Int -> IO (Either WeiboError [Status])
myGetStatus cookie containerID mbPage = do
  let opts =
        Wreq.defaults & Wreq.header "Accept" .~
        [BS8.fromString "application/json"] &
        Wreq.header "Cookie" .~
        [T.encodeUtf8 cookie] &
        Wreq.param "containerid" .~
        [containerID]
  resp <-
    Wreq.getWith
      (opts & Wreq.param "page" .~ (map (T.pack . show) (maybeToList mbPage)))
      (weiboUrl "/api/container/getIndex")
  case resp ^. Wreq.responseStatus . Wreq.statusCode of
    200 ->
      let ei =
            eitherDecode (resp ^. Wreq.responseBody) :: Either String StatusListResponse
       in return $ Bi.bimap WeiboParseError (view #statuses) ei
    _ -> return (Left (WeiboWreqError resp))

newWeiboApiClient :: Cookie -> Text -> IO (WeiboApiClient WeiboM)
newWeiboApiClient cookie containerID = do
  settings <- newManager tlsManagerSettings
  let clientEnv =
        Servant.mkClientEnv
          settings
          (Servant.BaseUrl Servant.Https "m.weibo.cn" 443 "")
      getStatusesM :<|> getCommentsM = Servant.client weiboApi (Just cookie)
      --
      weiboApiGetStatuses :: Maybe Int -> WeiboM [Status]
      weiboApiGetStatuses mbPage =
        liftWeibo (myGetStatus (unCookie cookie) containerID mbPage)
      --
      weiboApiGetComments :: StatusID -> Maybe Int -> WeiboM [Comment]
      weiboApiGetComments statusID mbPage =
        liftWeibo $ Bi.bimap WeiboServantError (view #comments) <$>
        Servant.runClientM (getCommentsM (Just statusID) mbPage) clientEnv
      --
      weiboApiDownloadPicture :: PictureID -> WeiboM Picture
      weiboApiDownloadPicture pid =
        liftWeibo $ do
          resp <- Wreq.get (largeJpgUrl pid)
          if (resp ^. Wreq.responseStatus . Wreq.statusCode /= 200)
            then return (Left (WeiboWreqError resp))
            else let identifier = pid
                     bytes = Just (resp ^. Wreq.responseBody)
                  in return (Right Picture {identifier, bytes})
  return
    WeiboApiClient
      {weiboApiGetStatuses, weiboApiGetComments, weiboApiDownloadPicture}

largeJpgUrl :: PictureID -> String
largeJpgUrl (ID pid) =
  BS8.toString $ "https://ww1.sinaimg.cn/large/" <>
  HTTP.urlEncode True (T.encodeUtf8 pid) <>
  ".jpg"

getComments :: MonadWeibo m => StatusID -> Maybe Int -> m [Comment]
getComments statusID mbPage = do
  WeiboApiClient {weiboApiGetComments} <- ask
  weiboApiGetComments statusID mbPage

getStatuses :: MonadWeibo m => Maybe Int -> m [Status]
getStatuses mbPage = do
  WeiboApiClient {weiboApiGetStatuses} <- ask
  weiboApiGetStatuses mbPage

downloadPicture :: MonadWeibo m => PictureID -> m Picture
downloadPicture pictureID = do
  WeiboApiClient {weiboApiDownloadPicture} <- ask
  weiboApiDownloadPicture pictureID
