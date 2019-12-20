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

internalDefaultLanguage :: LanguageCode
internalDefaultLanguage = "eng"

configFileLanguage :: FilePath -> IO String
configFileLanguage fp = do
    rv <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP fp
        return =<< get cp "eng" "lang"
    case rv of
        Left  _    -> return internalDefaultLanguage
        Right lang -> return lang

defaultLanguage :: IO LanguageCode
defaultLanguage = do
    fp <- configFilePath
    configFileExists <- doesFileExist fp
    if not configFileExists
        then return "eng"
        else return =<< configFileLanguage fp

configParser :: Parser Config
configParser = Config
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help ("Language code used to query open subtitles"
                                                                     ++ "\t(Defaults to english if it is not provided)"))
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  $ fullDesc <> progDesc "Search subtitles on open subtitles using the file's hash"
             <> header   "Hashkell Subtitles"

main :: IO ()
main = do
    commandLineConf <- execParser opts
    manager <- newManager defaultManagerSettings
    forM_  (files commandLineConf) $ \fn -> do
        movie <- readMovie fn
        case languageCode commandLineConf of
            Nothing -> defaultLanguage >>= downloadSubtitles manager SearchByHash movie
            Just l  -> downloadSubtitles manager SearchByHash movie l

