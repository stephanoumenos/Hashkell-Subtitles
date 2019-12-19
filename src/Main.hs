module Main where

import Control.Monad.Error
import Data.ConfigFile
import Hashkell.Types      (readMovie, Mode(SearchByHash), LanguageCode)
import Hashkell.Network    (downloadSubtitles)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative
import System.Directory (getHomeDirectory, doesFileExist)

data Config = Config { languageCode :: Maybe LanguageCode
                     , files        :: [FilePath]
                     } deriving Show

configFilePath :: IO FilePath
configFilePath = do
    homeDirectory <- liftIO getHomeDirectory
    return $ homeDirectory ++ "/.config/hashkell-subtitles/config"

configFileLanguage :: FilePath -> IO (Either CPError String)
configFileLanguage fp = runErrorT $ do
    cp <- join $ liftIO $ readfile emptyCP fp
    return =<< get cp "eng" "lang"

defaultLanguage :: IO LanguageCode
defaultLanguage = do
    fp <- configFilePath
    configFileExists <- doesFileExist fp
    if not configFileExists
        then return "eng"
        else do
            configFileLang <- configFileLanguage fp
            case configFileLang of
                Left _ -> return "eng"
                Right lang -> return lang

configParser :: Parser Config
configParser = Config
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help ("Language code used to query open subtitles"
                                                                     ++ "\t(Defaults to english if it is not provided)"))
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  $ fullDesc <> progDesc "Search subtitles on open subtitles using the file's hash"
             <> header   "Hashkell Subtitles"


readMovieAndDownloadSubtitles :: Maybe LanguageCode -> FilePath -> IO ()
readMovieAndDownloadSubtitles Nothing fn     = do
    userDefaultLang <- defaultLanguage
    readMovieAndDownloadSubtitles (Just userDefaultLang) fn
readMovieAndDownloadSubtitles (Just lang) fn = do
    manager <- newManager defaultManagerSettings
    movie <- readMovie fn
    downloadSubtitles SearchByHash movie lang manager

main :: IO ()
main = do
    commandLineConf <- execParser opts
    mapM_ (readMovieAndDownloadSubtitles $ languageCode commandLineConf) (files commandLineConf)



