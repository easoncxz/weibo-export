module API.Types
  ( Status(..)
  , User(..)
  , Comment(..)
  , Picture(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.API

data Status = Status
  { statusID :: Text
  , statusCreatedAt :: Text
  , statusText :: Text
  , statusPicIDs :: [Text]
  , statusUser :: User
  , statusRetweetedStatus :: Maybe Status
  , statusCommentsCount :: Int
  , statusRawJSON :: Value
  } deriving (Show)

instance FromJSON Status where
  parseJSON =
    withObject "status object" $ \o -> do
      statusID <- o .: "idstr"
      statusCreatedAt <- o .: "created_at"
      statusText <- o .: "text"
      statusPicIDs <- o .: "pic_ids"
      statusUser <- o .: "user"
      statusRetweetedStatus <- o .: "retweeted_status"
      statusCommentsCount <- o .: "comments_count"
      let statusRawJSON = Object o
      return Status {..}

data User = User
  { userID :: Int
  , userScreenName :: Text
  , userProfileImageURL :: Text
  , userRawJSON :: Value
  } deriving (Show)

instance FromJSON User where
  parseJSON =
    withObject "user object" $ \o -> do
      userID <- o .: "id"
      userScreenName <- o .: "screen_name"
      userProfileImageURL <- o .: "profile_image_url"
      let userRawJSON = Object o
      return User {..}

data Comment = Comment
  { commentID :: Text
  , commentUser :: User
  , commentCreatedAt :: Text
  , commentSource :: Text
  , commentText :: Text
  , commentReplyID :: Text
  , commentReplyText :: Text
  , commentRawJSON :: Value
  } deriving (Show)

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

data Picture =
  Picture
  deriving (Show)

newtype PageResponse =
  PageResponse [Status]
  deriving (Show)

instance FromJSON PageResponse where
  parseJSON =
    withObject "a mod/page_list response" $ \o -> do
      cards :: [Object] <- o .: "card_group"
      statuses <- sequence [c .: "mblog" | c <- cards]
      return (PageResponse statuses)
