{-# LANGUAGE OverloadedLabels #-}

module Weibo.Serialisation where

import Control.Applicative
import Control.Lens (view)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Generics.Labels ()
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
      o .:? "deleted" >>= \case
        Nothing ->
          StatusNormal <$> parseJSON (Object o) <|>
          StatusDeleted <$> parseJSON (Object o)
        Just ("1" :: Text) -> StatusDeleted <$> parseJSON (Object o)
        Just other -> fail $ "Unrecognised value at $.deleted: " ++ show other

instance FromJSON NormalStatus where
  parseJSON =
    withObject "NormalStatus" $ \o -> do
      let rawJSON = Object o
      identifier <- o .: "idstr"
      createdAt <- o .: "created_at"
      text <- o .: "text"
      picIDs <- o .:? "pic_ids" .!= []
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

newtype StatusListResponse =
  StatusListResponse
    { statuses :: [Status]
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
