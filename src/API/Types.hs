module API.Types
  ( ID(..)
  , Status(..)
  , _TagNormalStatus
  , _TagDeletedStatus
  , StatusID
  , NormalStatus(..)
  , DeletedStatus(..)
  , User(..)
  , UserID
  , Comment(..)
  , CommentID
  , Picture(..)
  , PictureID
  , StatusListResponse(..)
  , CommentListResponse(..)
  , bytes
  , comments
  , commentsCount
  , createdAt
  , identifier
  , picIDs
  , profileImageURL
  , rawJSON
  , replyID
  , replyText
  , retweetedStatus
  , screenName
  , source
  , statuses
  , text
  , user
  ) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Vector as V
import Servant.API

newtype ID a i = ID
  { getID :: i
  } deriving (Eq, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data Picture = Picture
  { _pictureIdentifier :: PictureID
  , _pictureBytes :: Maybe BSL.ByteString
  } deriving (Eq)

type PictureID = ID Picture Text

makeFields ''Picture

instance Show Picture where
  show Picture {_pictureIdentifier = ID t, _pictureBytes = b} =
    "Picture { _pictureIdentifier = " ++
    show t ++ ", _pictureBytes = <" ++ show (BSL.length <$> b) ++ " bytes> }"

instance ToJSON Picture where
  toJSON Picture {_pictureIdentifier} =
    object ["pictureID" .= toJSON _pictureIdentifier]

instance FromJSON Picture where
  parseJSON =
    withObject "Picture" $ \o -> Picture <$> (o .: "pictureID") <*> pure Nothing

data User = User
  { _userIdentifier :: UserID
  , _userScreenName :: Text
  , _userProfileImageURL :: Text
  , _userRawJSON :: Value
  } deriving (Eq, Show)

type UserID = ID User Integer

makeFields ''User

instance FromJSON User where
  parseJSON =
    withObject "user object" $ \o -> do
      _userIdentifier <- o .: "id"
      _userScreenName <- o .: "screen_name"
      _userProfileImageURL <- o .: "profile_image_url"
      let _userRawJSON = Object o
      return User {..}

instance ToJSON User where
  toJSON = toJSON . _userRawJSON

data RetweetedStatus = RetweetedStatus
  { _retweetedStatusIdentifier :: StatusID
  , _retweetedStatusText :: Text
  , _retweetedStatusRawJSON :: Value
  } deriving (Eq, Show)

data NormalStatus = NormalStatus
  { _normalStatusIdentifier :: StatusID
  , _normalStatusCreatedAt :: Text
  , _normalStatusText :: Text
  , _normalStatusPicIDs :: [PictureID]
  , _normalStatusUser :: User
  , _normalStatusRetweetedStatus :: Maybe RetweetedStatus
  , _normalStatusCommentsCount :: Int
  , _normalStatusRawJSON :: Value
  } deriving (Eq, Show)

data DeletedStatus = DeletedStatus
  { _deletedStatusIdentifier :: StatusID
  , _deletedStatusCreatedAt :: Text
  , _deletedStatusRawJSON :: Value
  } deriving (Eq, Show)

data Status
  = TagNormalStatus NormalStatus
  | TagDeletedStatus DeletedStatus
  deriving (Eq, Show)

type StatusID = ID Status Text

makeFields ''NormalStatus

makeFields ''DeletedStatus

makeFields ''RetweetedStatus

makePrisms ''Status

instance FromJSON DeletedStatus where
  parseJSON =
    withObject "DeletedStatus" $ \o -> do
      let _deletedStatusRawJSON = Object o
      _deletedStatusIdentifier <- o .: "idstr"
      _deletedStatusCreatedAt <- o .: "created_at"
      return DeletedStatus {..}

instance FromJSON RetweetedStatus where
  parseJSON =
    withObject "RetweetedStatus" $ \o -> do
      let _retweetedStatusRawJSON = Object o
      _retweetedStatusIdentifier <- o .: "idstr"
      _retweetedStatusText <- o .: "text"
      return RetweetedStatus {..}

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
        Nothing -> TagNormalStatus <$> parseJSON (Object o)
        Just ("1" :: Text) -> TagDeletedStatus <$> parseJSON (Object o)
        Just other -> fail $ "Unrecognised value at $.deleted: " ++ show other

instance ToJSON DeletedStatus where
  toJSON = toJSON . view rawJSON

instance ToJSON NormalStatus where
  toJSON = toJSON . view rawJSON

instance ToJSON Status where
  toJSON =
    \case
      TagDeletedStatus s -> toJSON s
      TagNormalStatus s -> toJSON s

data Comment = Comment
  { _commentIdentifier :: CommentID
  , _commentUser :: User
  , _commentCreatedAt :: Text
  , _commentSource :: Text
  , _commentText :: Text
  , _commentReplyID :: Maybe CommentID
  , _commentReplyText :: Maybe Text
  , _commentRawJSON :: Value
  } deriving (Eq, Show)

type CommentID = ID Comment Integer

makeFields ''Comment

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
  toJSON = toJSON . view rawJSON

newtype StatusListResponse = StatusListResponse
  { _statusListResponseStatuses :: [Status]
  } deriving (Eq, Show)

makeFields ''StatusListResponse

instance FromJSON StatusListResponse where
  parseJSON = do
    withArray "a one-element array" $ \a ->
      V.headM a >>=
      (withObject "a mod/page_list response" $ \o -> do
         cards :: [Object] <- o .: "card_group"
         ss <- sequence [c .: "mblog" | c <- cards]
         return (StatusListResponse ss))

newtype CommentListResponse = CommentListResponse
  { _commentListResponseComments :: [Comment]
  } deriving (Show)

makeFields ''CommentListResponse

instance FromJSON CommentListResponse where
  parseJSON =
    withObject "a response holding some comments" $
    return . CommentListResponse <=< (.: "data") <=< (.: "data")
