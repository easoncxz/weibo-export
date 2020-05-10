{-# LANGUAGE OverloadedLabels #-}

module Weibo.Serialisation where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Labels ()
import Data.String.ToString
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ID a i =
  ID
    { getID :: i
    }
  deriving (Eq, Show, ToString, FromJSON, ToJSON, Generic)

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
