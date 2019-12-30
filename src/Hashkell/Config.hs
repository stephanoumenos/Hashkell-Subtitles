module Hashkell.Config ( execParser
                       , commandLineOptions
                       , defaultLanguage
                       , CommandLineConfig(..)
                       ) where

import Hashkell.Types (LanguageCode)

import Control.Monad.Except (liftIO, join, runExceptT)
import Data.ConfigFile
import Options.Applicative
import System.Directory    (getHomeDirectory, doesFileExist)

data CommandLineConfig = CommandLineConfig { languageCode :: Maybe LanguageCode
                                           , sequential   :: Maybe Bool
                                           , files        :: [FilePath]
                                           }

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

languageHelpMessage :: String
languageHelpMessage = "Language code used to query open subtitles"
                   ++ "\t(Defaults to english if it is not provided)"

sequentialHelpMessage :: String
sequentialHelpMessage = "If present program will download subtitles sequentially i.e. not use async"

configParser :: Parser CommandLineConfig
configParser = CommandLineConfig
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help languageHelpMessage)
    <*> (optional . switch $ long "sequential" <> short 's' <> help sequentialHelpMessage)
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

commandLineOptions :: ParserInfo CommandLineConfig
commandLineOptions = info (configParser <**> helper)
  $ fullDesc <> progDesc ("Search subtitles on open subtitles using the file's hash."
                    ++ " This program uses async for default, use -s if you'd prefer a sequential behavior")
             <> header   "Hashkell Subtitles"

