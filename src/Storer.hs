module Storer where

import Data.Aeson (Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Weibo.Serialisation

filenameForStatus :: Status -> FilePath
filenameForStatus status =
  case status of
    StatusNormal (NormalStatus {identifier = ID i}) ->
      "status-normal-" <> T.unpack i <> ".json"
    StatusDeleted (DeletedStatus {identifier = ID i}) ->
      "status-deleted-" <> T.unpack i <> ".json"

saveStatus :: FilePath -> Status -> IO FilePath
saveStatus dir status = do
  createDirectoryIfMissing True dir
  let filename = filenameForStatus status
      path = (dir </> filename)
  BSL.writeFile path (encodePretty status)
  return path

saveUnrecognisableStatusListResponse :: FilePath -> Text -> Value -> IO FilePath
saveUnrecognisableStatusListResponse dir pageTag weird = do
  createDirectoryIfMissing True dir
  let filename =
        "statuslist-unrecognisable-page-" <> T.unpack pageTag <> ".json"
      path = (dir </> filename)
  BSL.writeFile path (encodePretty weird)
  return path
