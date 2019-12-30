module Hashkell.Config ( execParser
                       , commandLineOptions
                       , defaultLanguage
                       , providedVideoFiles
                       , CommandLineConfig(..)
                       ) where

import Hashkell.Types (LanguageCode)

import           Control.Monad.Except (liftIO, join, runExceptT)
import           Data.ConfigFile
import qualified Data.Set as         S(fromList, member, Set)
import           Options.Applicative
import           System.Directory     (getHomeDirectory, doesFileExist)
import           System.FilePath      (takeExtension)
import           System.FilePath.Find

-- Config File Config --

configFilePath :: IO FilePath
configFilePath = do
    homeDirectory <- liftIO getHomeDirectory
    return $ homeDirectory ++ "/.config/hashkell-subtitles/config"

internalDefaultLanguage :: LanguageCode
internalDefaultLanguage = "eng"

configFileLanguage :: FilePath -> IO String
configFileLanguage fp = do
    rv <- runExceptT $ do
        cp <- join $ liftIO $ readfile emptyCP fp
        get cp "eng" "lang"
    case rv of
        Left  _    -> return internalDefaultLanguage
        Right lang -> return lang

defaultLanguage :: IO LanguageCode
defaultLanguage = do
    fp <- configFilePath
    configFileExists <- doesFileExist fp
    if not configFileExists
        then return internalDefaultLanguage
        else configFileLanguage fp

-- Comand Line Config --

data CommandLineConfig = CommandLineConfig { languageCode     :: Maybe LanguageCode
                                           , sequential       :: Maybe Bool
                                           , recursive        :: Maybe Bool
                                           , files            :: [FilePath]
                                           }

languageHelpMessage :: String
languageHelpMessage = "Language code used to query open subtitles"
                   ++ "\t(Defaults to english if it is not provided)"

sequentialHelpMessage :: String
sequentialHelpMessage = "If present program will download subtitles sequentially i.e. not use async"

recursiveHelpMessage :: String
recursiveHelpMessage = "Will try to find files recursively"

configParser :: Parser CommandLineConfig
configParser = CommandLineConfig
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help languageHelpMessage)
    <*> (optional . switch $ long "sequential" <> short 's' <> help sequentialHelpMessage)
    <*> (optional . switch $ long "recursive" <> short 'r' <> help recursiveHelpMessage)
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

commandLineOptions :: ParserInfo CommandLineConfig
commandLineOptions = info (configParser <**> helper)
  $ fullDesc <> progDesc ("Search subtitles on open subtitles using the file's hash."
                    ++ " This program uses async for default, use -s if you'd prefer a sequential behavior")
             <> header   "Hashkell Subtitles"

videoExtensions :: S.Set String
videoExtensions = S.fromList
                  [ ".3g2", ".3gp", ".3gp2", ".3gpp", ".60d", ".ajp", ".asf", ".asx", ".avchd", ".avi", ".bik"
                  , ".bix", ".box", ".cam", ".dat", ".divx", ".dmf", ".dv", ".dvr-ms", ".evo", ".flc", ".fli"
                  , ".flic", ".flv", ".flx", ".gvi", ".gvp", ".h264", ".m1v", ".m2p", ".m2ts", ".m2v", ".m4e"
                  , ".m4v", ".mjp", ".mjpeg", ".mjpg", ".mkv", ".moov", ".mov", ".movhd", ".movie", ".movx", ".mp4"
                  , ".mpe", ".mpeg", ".mpg", ".mpv", ".mpv2", ".mxf", ".nsv", ".nut", ".ogg", ".ogm", ".ogv", ".omf"
                  , ".ps", ".qt", ".ram", ".rm", ".rmvb", ".swf", ".ts", ".vfw", ".vid", ".video", ".viv", ".vivo"
                  , ".vob", ".vro", ".wm", ".wmv", ".wmx", ".wrap", ".wvx", ".wx", ".x264", ".xvid"
                  ]

isVideoExtension :: FilePath -> Bool
isVideoExtension fp = S.member (takeExtension fp) videoExtensions

recursivelyFindVideoFiles :: CommandLineConfig -> IO [FilePath]
recursivelyFindVideoFiles c =
    filter isVideoExtension . join <$> mapM (find always (fileType ==? RegularFile)) (files c)

providedVideoFiles :: CommandLineConfig -> IO [FilePath]
providedVideoFiles c =
    if recursive c == Just True
        then recursivelyFindVideoFiles c
        else return $ filter isVideoExtension (files c)

