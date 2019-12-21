module Hashkell.Config ( execParser
                       , commandLineOptions
                       , defaultLanguage
                       , CommandLineConfig(languageCode, files)
                       ) where

import Hashkell.Types (LanguageCode)

import Control.Monad.Error
import Data.ConfigFile
import Options.Applicative
import System.Directory    (getHomeDirectory, doesFileExist)

data CommandLineConfig = CommandLineConfig { languageCode :: Maybe LanguageCode
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

configParser :: Parser CommandLineConfig
configParser = CommandLineConfig
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help ("Language code used to query open subtitles"
                                                                     ++ "\t(Defaults to english if it is not provided)"))
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

commandLineOptions :: ParserInfo CommandLineConfig
commandLineOptions = info (configParser <**> helper)
  $ fullDesc <> progDesc "Search subtitles on open subtitles using the file's hash"
             <> header   "Hashkell Subtitles"

