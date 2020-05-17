{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI where

import Control.Monad.IO.Class (liftIO)
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (ExitCode(..), exitWith)

import Lib (downloadAndSaveStatusesSimultaneously)
import Paths_weibo_export (version)
import Weibo (makeWeiboApiClient, runWeiboM)
import Weibo.Serialisation (ID(ID))

data CLIMode
  = DownloadStatuses
      { weiboUserID :: Integer
      , startFromPage :: Int
      , saveDir :: FilePath
      , noWait :: Bool
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
      weiboUserID <-
        option
          auto
          (short 'u' <>
           long "user" <> help "User ID on weibo, e.g. \"3563717322\"")
      startFromPage <-
        option
          auto
          (short 'p' <>
           long "start-from-page" <>
           help "Start downloading from this page" <> showDefault <> value 1)
      saveDir <-
        strOption
          (short 'o' <>
           long "output-dir" <>
           help
             "Where to put downloaded files. This will be created with `mkdir -p` if it doesn't exist.")
      noWait <-
        switch
          (short 'w' <>
           long "no-wait" <>
           help "Don't wait, be aggressive, and risk being rate-limited")
      pure DownloadStatuses {..}

cliModeInfo :: ParserInfo CLIMode
cliModeInfo =
  info
    (cliModeParser <**> helper)
    (fullDesc <>
     progDesc "Download Weibo statuses via the m.weibo.cn mobile web API" <>
     header ("weibo-export: " <> (showVersion version)))

runApp :: CLIMode -> IO ()
runApp =
  \case
    ShowVersion -> putStrLn (showVersion version)
    DownloadStatuses {weiboUserID, startFromPage, saveDir, noWait} -> do
      client <- makeWeiboApiClient
      runWeiboM
        client
        (downloadAndSaveStatusesSimultaneously
           saveDir
           noWait
           (ID weiboUserID)
           startFromPage) >>= \case
        Left e ->
          liftIO $ do
            putStrLn ("Error (probably with the network): " <> show e)
            exitWith (ExitFailure 15)
        Right () -> return ()

cliMain :: IO ()
cliMain = do
  mode <- execParser cliModeInfo
  runApp mode
