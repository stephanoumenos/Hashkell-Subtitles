module Main where

import Hashkell.Types      (readMovie, Mode(SearchByHash), LanguageCode)
import Hashkell.Network    (downloadSubtitles)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative

data Config = Config { languageCode :: Maybe LanguageCode
                     , files        :: [FilePath]
                     } deriving Show

configParser :: Parser Config
configParser = Config
    <$> (optional . strOption $ long "language-code" <> short 'l' <> help ("Language code used to query open subtitles"
                                                                     ++ "\t(Defaults to english if it is not provided)"))
    <*> some (argument str $ metavar "FILES..." <> help "Files to search subtitles for")

opts :: ParserInfo Config
opts = info (configParser <**> helper)
  $ fullDesc <> progDesc "Search subtitles on open subtitles using the file's hash"
             <> header   "Hashkell Subtitles"

defaultLanguage :: LanguageCode
defaultLanguage = "eng"

readMovieAndDownloadSubtitles :: Maybe LanguageCode -> FilePath -> IO ()
readMovieAndDownloadSubtitles Nothing fn     = readMovieAndDownloadSubtitles (Just defaultLanguage) fn
readMovieAndDownloadSubtitles (Just lang) fn = do
    manager <- newManager defaultManagerSettings
    movie <- readMovie fn
    downloadSubtitles SearchByHash movie lang manager

main :: IO ()
main = do
    conf <- execParser opts
    print conf
    mapM_ (readMovieAndDownloadSubtitles $ languageCode conf) (files conf)

