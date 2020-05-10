{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import Lib (downloadAndSaveStatusesSimultaneously)
import Paths_weibo_export (version)
import Weibo (makeWeiboApiClient, runWeiboM)

data CLIMode
  = DownloadStatuses
      { saveDir :: FilePath
      , headersFilePath :: FilePath
      , weiboContainerID :: Text
      }
  | ShowVersion
  deriving (Eq, Show)

cliModeParser :: Parser CLIMode
cliModeParser = versionParser <|> downloadStatusParser
  where
    versionParser =
      flag'
        ShowVersion
        (long "version" <>
         short 'v' <> help "Display the version of weibo-export")
    downloadStatusParser = do
      saveDir <-
        strOption
          (short 'o' <>
           long "output-dir" <>
           help
             "Where to put downloaded files. This will be created with `mkdir -p` if it doesn't exist.")
      headersFilePath <-
        strOption
          (short 'f' <>
           long "headers-file" <>
           help
             "A JSON file containing all HTTP headers to use. Format is Firefox's \"Copy Request Headers\".")
      weiboContainerID <-
        strOption
          (short 'i' <>
           long "container-id" <>
           help
             "The `containerid` GET query param, copied verbatim from your browser.")
      pure DownloadStatuses {..}

cliModeInfo :: ParserInfo CLIMode
cliModeInfo =
  info
    (cliModeParser <**> helper)
    (fullDesc <>
     progDesc "Download Weibo statuses via the m.weibo.cn mobile web API" <>
     header "weibo-export")

runApp :: CLIMode -> IO ()
runApp =
  \case
    ShowVersion -> putStrLn ("weibo-export: " <> (showVersion version))
    DownloadStatuses {saveDir, headersFilePath, weiboContainerID} -> do
      client <- makeWeiboApiClient weiboContainerID headersFilePath
      runWeiboM client (downloadAndSaveStatusesSimultaneously saveDir) >>= \case
        Left e ->
          liftIO $ do
            putStrLn ("Error (probably with the network): " <> show e)
            exitWith (ExitFailure 15)
        Right () -> return ()

cliMain :: IO ()
cliMain = do
  mode <- execParser cliModeInfo
  runApp mode
