{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI where

import Data.Text (Text)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import Lib (downloadAndSaveStatuses)
import Paths_weibo_export (version)
import Weibo (makeWeiboApiClient, runWeiboM)

data CLIMode
  = DownloadStatuses
      { saveDir :: FilePath
      , cookie :: Text
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
      cookie <-
        strOption
          (short 'c' <>
           long "cookie" <>
           help
             "Copy the `Cookie` HTTP request header value verbatim from your browser's developer console")
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
    DownloadStatuses {saveDir, cookie, weiboContainerID} -> do
      let client = makeWeiboApiClient cookie weiboContainerID
      runWeiboM client (downloadAndSaveStatuses saveDir) >>= \case
        Left e -> do
          putStrLn ("Error (probably with the network): " <> show e)
          exitWith (ExitFailure 15)
        Right save -> do
          save
          putStrLn "All done."

cliMain :: IO ()
cliMain = do
  mode <- execParser cliModeInfo
  runApp mode
