module Main where

import Hashkell.Config     (execParser, commandLineOptions, CommandLineConfig(languageCode, files))
import Hashkell.Types      (readMovie, Mode(SearchByHash))
import Hashkell.Network    (downloadSubtitles)

import Control.Monad       (forM_)
import Network.HTTP.Client (defaultManagerSettings, newManager)

main :: IO ()
main = do
    commandLineConf <- execParser commandLineOptions
    manager <- newManager defaultManagerSettings
    forM_  (files commandLineConf) $ \fn -> do
        movie <- readMovie fn
        downloadSubtitles manager SearchByHash movie (languageCode commandLineConf)
