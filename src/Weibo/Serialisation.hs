{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Weibo.Serialisation where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.String.ToString
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Servant.API

newtype ID a i =
  ID
    { getID :: i
    }
  deriving ( Eq
           , Show
           , ToString
           , FromJSON
           , ToJSON
           , FromHttpApiData
           , ToHttpApiData
           , Generic
           )

data Picture =
  Picture
    { identifier :: PictureID
    , bytes :: Maybe BSL.ByteString
    }
  deriving (Eq, Generic)

type PictureID = ID Picture Text

instance Show Picture where
  show Picture {identifier = ID t, bytes = b} =
    "Picture { identifier = " ++
    show t ++ ", bytes = <" ++ show (BSL.length <$> b) ++ " bytes> }"

instance ToJSON Picture where
  toJSON Picture {identifier} = object ["pictureID" .= toJSON identifier]

instance FromJSON Picture where
  parseJSON =
    withObject "Picture" $ \o -> Picture <$> (o .: "pictureID") <*> pure Nothing

data User =
  User
    { identifier :: UserID
    , _userScreenName :: Text
    , _userProfileImageURL :: Text
    , _userRawJSON :: Value
    }
  deriving (Eq, Show, Generic)

type UserID = ID User Integer

instance FromJSON User where
  parseJSON =
    withObject "user object" $ \o -> do
      identifier <- o .: "id"
      _userScreenName <- o .: "screen_name"
      _userProfileImageURL <- o .: "profile_image_url"
      let _userRawJSON = Object o
      return User {..}

instance ToJSON User where
  toJSON = toJSON . _userRawJSON

data NormalStatus =
  NormalStatus
    { _normalStatusIdentifier :: StatusID
    , _normalStatusCreatedAt :: Text
    , _normalStatusText :: Text
    , _normalStatusPicIDs :: [PictureID]
    , _normalStatusUser :: User
    , _normalStatusRetweetedStatus :: Maybe Status
    , _normalStatusCommentsCount :: Int
    , _normalStatusRawJSON :: Value
    }
  deriving (Eq, Show, Generic)

data DeletedStatus =
  DeletedStatus
    { _deletedStatusIdentifier :: StatusID
    , _deletedStatusCreatedAt :: Text
    , _deletedStatusRawJSON :: Value
    }
  deriving (Eq, Show, Generic)

data Status
  = TagNormalStatus NormalStatus
  | TagDeletedStatus DeletedStatus
  deriving (Eq, Show, Generic)

type StatusID = ID Status Text

instance FromJSON DeletedStatus where
  parseJSON =
    withObject "DeletedStatus" $ \o -> do
      let _deletedStatusRawJSON = Object o
      _deletedStatusIdentifier <- o .: "idstr"
      _deletedStatusCreatedAt <- o .: "created_at"
      return DeletedStatus {..}

instance FromJSON NormalStatus where
  parseJSON =
    withObject "NormalStatus" $ \o -> do
      let _normalStatusRawJSON = Object o
      _normalStatusIdentifier <- o .: "idstr"
      _normalStatusCreatedAt <- o .: "created_at"
      _normalStatusText <- o .: "text"
      _normalStatusPicIDs <- o .:? "pic_ids" .!= []
      _normalStatusUser <- o .: "user"
      _normalStatusRetweetedStatus <- o .:? "retweeted_status"
      _normalStatusCommentsCount <- o .: "comments_count"
      return NormalStatus {..}

instance FromJSON Status where
  parseJSON =
    withObject "Status" $ \o ->
      o .:? "deleted" >>= \case
        Nothing ->
          TagNormalStatus <$> parseJSON (Object o) <|>
          TagDeletedStatus <$> parseJSON (Object o)
        Just ("1" :: Text) -> TagDeletedStatus <$> parseJSON (Object o)
        Just other -> fail $ "Unrecognised value at $.deleted: " ++ show other

instance ToJSON DeletedStatus where
  toJSON = toJSON . _deletedStatusRawJSON

instance ToJSON NormalStatus where
  toJSON = toJSON . _normalStatusRawJSON

instance ToJSON Status where
  toJSON =
    \case
      TagDeletedStatus s -> toJSON s
      TagNormalStatus s -> toJSON s

data Comment =
  Comment
    { _commentIdentifier :: CommentID
    , _commentUser :: User
    , _commentCreatedAt :: Text
    , _commentSource :: Text
    , _commentText :: Text
    , _commentReplyID :: Maybe CommentID
    , _commentReplyText :: Maybe Text
    , _commentRawJSON :: Value
    }
  deriving (Eq, Show, Generic)

type CommentID = ID Comment Integer

instance FromJSON Comment where
  parseJSON =
    withObject "comment object" $ \o -> do
      _commentIdentifier <- o .: "id"
      _commentCreatedAt <- o .: "created_at"
      _commentSource <- o .: "source"
      _commentUser <- o .: "user"
      _commentText <- o .: "text"
      _commentReplyID <- o .:? "reply_id"
      _commentReplyText <- o .:? "reply_text"
      let _commentRawJSON = Object o
      return Comment {..}

instance ToJSON Comment where
  toJSON = toJSON . _commentRawJSON

newtype StatusListResponse =
  StatusListResponse
    { _statusListResponseStatuses :: [Status]
    }
  deriving (Eq, Show, Generic)

instance FromJSON StatusListResponse where
  parseJSON =
    withArray "a one-element array" $
    V.headM >=>
    withObject
      "a mod/page_list response"
      (\o -> do
         cards :: [Object] <- o .: "card_group"
         ss <- sequence [c .: "mblog" | c <- cards]
         return (StatusListResponse ss))

newtype CommentListResponse =
  CommentListResponse
    { _commentListResponseComments :: [Comment]
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
