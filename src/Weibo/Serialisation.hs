{-# LANGUAGE OverloadedLabels #-}

module Weibo.Serialisation where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except (liftEither, runExcept, throwError)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types (Parser)
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.String.ToString
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics (Generic)

newtype HeadersConfig =
  HeadersConfig
    { unHeadersConfig :: Map BS.ByteString BS.ByteString
    }
  deriving (Show)

instance FromJSON HeadersConfig where
  parseJSON =
    withObject
      "HeadersConfig"
      (\headers ->
         (headers .: "Request Headers (0 B)") >>=
         withObject
           "HeadersConfig.\"Request Headers (0 B)\""
           (\(reqHeaders :: Object) ->
              (reqHeaders .: "headers") >>=
              withArray
                "HeadersConfig.\"Request Headers (0 B)\""
                (\(hs :: Array) -> do
                   headerPairs :: V.Vector (Text, Text) <-
                     V.mapM parseOneHeader hs
                   return
                     (HeadersConfig
                        (Map.fromList
                           (map
                              (Bi.bimap T.encodeUtf8 T.encodeUtf8)
                              (V.toList headerPairs)))))))
    where
      parseOneHeader :: Value -> Parser (Text, Text)
      parseOneHeader =
        withObject "oneHeader" $ \h -> (,) <$> h .: "name" <*> h .: "value"

newtype ID a i =
  ID
    { getID :: i
    }
  deriving (Eq, Show, ToString, FromJSON, ToJSON, Generic)

-- | Some kind of opaque Weibo concept
data Container

type ContainerID = ID Container Text

data Picture =
  Picture
    { identifier :: PictureID
    , bytes :: Maybe BSL.ByteString
    }
  deriving (Eq, Generic)

type PictureID = ID Picture Text

instance Show Picture where
  show Picture {identifier = ID t, bytes = b} =
    "Picture { identifier = " ++ show t ++ ", bytes = <" ++
    show (BSL.length <$> b) ++
    " bytes> }"

instance ToJSON Picture where
  toJSON Picture {identifier} = object ["pictureID" .= toJSON identifier]

instance FromJSON Picture where
  parseJSON =
    withObject "Picture" $ \o -> Picture <$> (o .: "pictureID") <*> pure Nothing

data User =
  User
    { identifier :: UserID
    , screenName :: Text
    , profileImageURL :: Text
    , rawJSON :: Value
    }
  deriving (Eq, Show, Generic)

type UserID = ID User Integer

instance FromJSON User where
  parseJSON =
    withObject "user object" $ \o -> do
      identifier <- o .: "id"
      screenName <- o .: "screen_name"
      profileImageURL <- o .: "profile_image_url"
      let rawJSON = Object o
      return User {identifier, screenName, profileImageURL, rawJSON}

instance ToJSON User where
  toJSON = toJSON . view #rawJSON

newtype UserRoot =
  UserRoot Value
  deriving (Show)

-- | https://m.weibo.cn/api/container/getIndex\?type\=uid\&value\=$uid
parseContainerID :: UserRoot -> Either String ContainerID
parseContainerID (UserRoot root) =
  runExcept $ do
    ok :: Value <-
      case root ^? key "ok" of
        Just (Number 1) -> return root
        _ -> do
          utf8 <-
            liftEither
              (Bi.first show (T.decodeUtf8' (BSL.toStrict (encode root))))
          throwError ("Weibo returned JSON error: " ++ T.unpack utf8)
    tabs :: Array <-
      case ok ^? key "data" . key "tabsInfo" . key "tabs" . _Array of
        Just vec -> return vec
        _ ->
          throwError
            ("Can't find 'tabs' array in Weibo API JSON: " ++ show (encode ok))
    case join
           (listToMaybe
              [ t ^? key "containerid" . _String
              | t <- toList tabs
              , t ^? key "tab_type" == Just (String "weibo")
              ]) of
      Just (cid :: Text) -> return (ID cid)
      Nothing ->
        throwError
          ("Cannot find a tab with tab_type == 'weibo': " ++ show (encode tabs))

data NormalStatus =
  NormalStatus
    { identifier :: StatusID
    , createdAt :: Text
    , text :: Text
    , picIDs :: [PictureID]
    , user :: User
    , retweetedStatus :: Maybe Status
    , commentsCount :: Int
    , rawJSON :: Value
    }
  deriving (Eq, Show, Generic)

data DeletedStatus =
  DeletedStatus
    { identifier :: StatusID
    , createdAt :: Text
    , rawJSON :: Value
    }
  deriving (Eq, Show, Generic)

data Status
  = StatusNormal NormalStatus
  | StatusDeleted DeletedStatus
  deriving (Eq, Show, Generic)

type StatusID = ID Status Text

instance FromJSON Status where
  parseJSON =
    withObject "Status" $ \o ->
      (StatusNormal <$> parseJSON (Object o)) <|>
      (StatusDeleted <$> parseJSON (Object o)) <|>
      (fail $ "Unrecognised value at $.deleted: " ++ show o)

instance FromJSON NormalStatus where
  parseJSON =
    withObject "NormalStatus" $ \o -> do
      let rawJSON = Object o
      identifier <- o .: "idstr"
      createdAt <- o .: "created_at"
      text <- o .: "text"
      let picIDs :: [PictureID] =
            fmap ID $ Object o ^.. key "pics" . _Array . traverse . key "pid" .
            _String
      user <- o .: "user"
      retweetedStatus <- o .:? "retweeted_status"
      commentsCount <- o .: "comments_count"
      return
        NormalStatus
          { identifier
          , createdAt
          , text
          , picIDs
          , user
          , retweetedStatus
          , commentsCount
          , rawJSON
          }

instance FromJSON DeletedStatus where
  parseJSON =
    withObject "DeletedStatus" $ \o -> do
      let rawJSON = Object o
      identifier <- o .: "idstr"
      createdAt <- o .: "created_at"
      return DeletedStatus {identifier, createdAt, rawJSON}

instance ToJSON Status where
  toJSON =
    \case
      StatusNormal s -> toJSON s
      StatusDeleted s -> toJSON s

instance ToJSON NormalStatus where
  toJSON = toJSON . view #rawJSON

instance ToJSON DeletedStatus where
  toJSON = toJSON . view #rawJSON

data Comment =
  Comment
    { identifier :: CommentID
    , user :: User
    , createAt :: Text
    , source :: Text
    , text :: Text
    , replyID :: Maybe CommentID
    , replyText :: Maybe Text
    , rawJSON :: Value
    }
  deriving (Eq, Show, Generic)

type CommentID = ID Comment Integer

instance FromJSON Comment where
  parseJSON =
    withObject "comment object" $ \o -> do
      identifier <- o .: "id"
      createAt <- o .: "created_at"
      source <- o .: "source"
      user <- o .: "user"
      text <- o .: "text"
      replyID <- o .:? "reply_id"
      replyText <- o .:? "reply_text"
      let rawJSON = Object o
      return
        Comment
          { identifier
          , createAt
          , source
          , user
          , text
          , replyID
          , replyText
          , rawJSON
          }

instance ToJSON Comment where
  toJSON = toJSON . view #rawJSON

data StatusListResponse
  = StatusListNormal [Status]
  | StatusListUnrecogniable Value
  deriving (Eq, Show, Generic)

instance FromJSON StatusListResponse where
  parseJSON v = parseNormal v <|> return (StatusListUnrecogniable v)
    where
      parseNormal =
        withObject "StatusListResponse" $ \statusListResponse -> do
          (n :: Integer) <- statusListResponse .: "ok"
          case n of
            1 -> do
              let (mblogs :: [Value]) =
                    Object statusListResponse ^.. key "data" . key "cards" .
                    _Array .
                    traverse .
                    key "mblog"
              (statuses :: [Status]) <- forM mblogs parseJSON
              return (StatusListNormal statuses)
            notOK -> fail $ "StatusListResponse.ok: " ++ show notOK

newtype CommentListResponse =
  CommentListResponse
    { comments :: [Comment]
    }
  deriving (Show, Generic)

instance FromJSON CommentListResponse where
  parseJSON =
    withObject "a response holding some comments" $ \outer ->
      (outer .: "ok") >>= \case
        (1 :: Int) ->
          (outer .: "data") >>= \inner ->
            CommentListResponse <$> (inner .: "data")
        e -> fail $ "CommentListResponse outer data failed, ok: " ++ show e
