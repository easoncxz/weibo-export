module API.Types
  ( ID(..)
  , Status(..)
  , StatusID
  , User(..)
  , UserID
  , Comment(..)
  , CommentID
  , Picture(..)
  , PictureID
  , StatusListResponse(..)
  , CommentListResponse(..)
  ) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Vector as V
import Servant.API

data ID a i = ID
  { unID :: i
  } deriving (Show)

instance (FromHttpApiData i) => FromHttpApiData (ID a i) where
  parseUrlPiece = fmap ID . parseUrlPiece

instance (ToHttpApiData i) => ToHttpApiData (ID a i) where
  toUrlPiece = toUrlPiece . unID

instance (FromJSON i) => FromJSON (ID a i) where
  parseJSON = fmap ID . parseJSON

instance (ToJSON i) => ToJSON (ID a i) where
  toJSON = toJSON . unID

instance (Eq i) => Eq (ID a i) where
  (==) = (==) `on` unID

type StatusID = ID Status Text

data Status
  = NormalStatus { normalStatusID :: StatusID
                 , normalStatusCreatedAt :: Text
                 , normalStatusText :: Text
                 , normalStatusPicIDs :: Maybe [PictureID]
                 , normalStatusUser :: User
                 , normalStatusRetweetedStatus :: Maybe Status
                 , normalStatusCommentsCount :: Int
                 , normalStatusRawJSON :: Value }
  | DeletedStatus { deletedStatusID :: StatusID
                  , deletedStatueCreatedAt :: Text
                  , deletedStatusRawJSON :: Value }
  deriving (Eq, Show)

instance FromJSON Status where
  parseJSON =
    withObject "status object" $ \o -> do
      let rawJSON = Object o
      idStr <- o .: "idstr"
      createdAt <- o .: "created_at"
      o .:? "deleted" >>= \case
        Just ("1" :: Text) -> return (DeletedStatus idStr createdAt rawJSON)
        Just other -> fail $ "Unrecognised value at $.deleted: " ++ show other
        Nothing -> do
          normalStatusText <- o .: "text"
          normalStatusPicIDs <- o .:? "pic_ids"
          normalStatusUser <- o .: "user"
          normalStatusRetweetedStatus <- o .:? "retweeted_status"
          normalStatusCommentsCount <- o .: "comments_count"
          let normalStatusID = idStr
              normalStatusCreatedAt = createdAt
              normalStatusRawJSON = rawJSON
          return NormalStatus {..}

instance ToJSON Status where
  toJSON NormalStatus {normalStatusRawJSON} = toJSON normalStatusRawJSON
  toJSON DeletedStatus {deletedStatusRawJSON} = toJSON deletedStatusRawJSON

type UserID = ID User Integer

data User = User
  { userID :: UserID
  , userScreenName :: Text
  , userProfileImageURL :: Text
  , userRawJSON :: Value
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON =
    withObject "user object" $ \o -> do
      userID <- o .: "id"
      userScreenName <- o .: "screen_name"
      userProfileImageURL <- o .: "profile_image_url"
      let userRawJSON = Object o
      return User {..}

type CommentID = ID Comment Integer

data Comment = Comment
  { commentID :: CommentID
  , commentUser :: User
  , commentCreatedAt :: Text
  , commentSource :: Text
  , commentText :: Text
  , commentReplyID :: Maybe CommentID
  , commentReplyText :: Maybe Text
  , commentRawJSON :: Value
  } deriving (Eq, Show)

instance FromJSON Comment where
  parseJSON =
    withObject "comment object" $ \o -> do
      commentID <- o .: "id"
      commentCreatedAt <- o .: "created_at"
      commentSource <- o .: "source"
      commentUser <- o .: "user"
      commentText <- o .: "text"
      commentReplyID <- o .:? "reply_id"
      commentReplyText <- o .:? "reply_text"
      let commentRawJSON = Object o
      return Comment {..}

instance ToJSON Comment where
  toJSON Comment {commentRawJSON} = toJSON commentRawJSON

data Picture = Picture
  { pictureID :: PictureID
  , pictureBytes :: Maybe BSL.ByteString
  } deriving (Eq)

type PictureID = ID Picture Text

instance Show Picture where
  show Picture {pictureID = ID t, pictureBytes = b} =
    "Picture { pictureID = " ++
    show t ++ ", pictureBytes = <" ++ show (BSL.length <$> b) ++ " bytes> }"

instance ToJSON Picture where
  toJSON Picture {pictureID} = object ["pictureID" .= toJSON pictureID]

instance FromJSON Picture where
  parseJSON =
    withObject "Picture" $ \o -> Picture <$> (o .: "pictureID") <*> pure Nothing

newtype StatusListResponse = StatusListResponse
  { statusListResponseStatuses :: [Status]
  } deriving (Eq, Show)

instance FromJSON StatusListResponse where
  parseJSON = do
    withArray "a one-element array" $ \a ->
      V.headM a >>=
      (withObject "a mod/page_list response" $ \o -> do
         cards :: [Object] <- o .: "card_group"
         statuses <- sequence [c .: "mblog" | c <- cards]
         return (StatusListResponse statuses))

newtype CommentListResponse = CommentListResponse
  { commentListResponseComments :: [Comment]
  } deriving (Show)

instance FromJSON CommentListResponse where
  parseJSON =
    withObject "a response holding some comments" $
    return . CommentListResponse <=< (.: "data") <=< (.: "data")
