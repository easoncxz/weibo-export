module API.Types
  ( ID(..)
  , Status(..)
  , StatusID
  , User(..)
  , UserID
  , Comment(..)
  , CommentID
  , Picture
  , StatusListResponse(..)
  , CommentListResponse(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Function ((&), on)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics
import Servant.API

data ID a i = ID
  { runID :: i
  } deriving (Show)

instance (FromHttpApiData i) => FromHttpApiData (ID a i) where
  parseUrlPiece = fmap ID . parseUrlPiece

instance (ToHttpApiData i) => ToHttpApiData (ID a i) where
  toUrlPiece = toUrlPiece . runID

instance (FromJSON i) => FromJSON (ID a i) where
  parseJSON = fmap ID . parseJSON

instance (Eq i) => Eq (ID a i) where
  (==) = (==) `on` runID

type StatusID = ID Status Text

data Status
  = NormalStatus { normalStatusID :: StatusID
                 , normalStatusCreatedAt :: Text
                 , normalStatusText :: Text
                 , normalStatusPicIDs :: Maybe [Text]
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

type UserID = ID User Int

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

type CommentID = ID Comment Text

data Comment = Comment
  { commentID :: CommentID
  , commentUser :: User
  , commentCreatedAt :: Text
  , commentSource :: Text
  , commentText :: Text
  , commentReplyID :: Text
  , commentReplyText :: Text
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
      commentReplyID <- o .: "reply_id"
      commentReplyText <- o .: "reply_text"
      let commentRawJSON = Object o
      return Comment {..}

data Picture

newtype StatusListResponse = StatusListResponse
  { unStatusListResponse :: [Status]
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
  { unCommentListResponse :: [Comment]
  } deriving (Show)

instance FromJSON CommentListResponse where
  parseJSON =
    withObject "a response holding some comments" $
    return . CommentListResponse <=< (.: "data") <=< (.: "data")
