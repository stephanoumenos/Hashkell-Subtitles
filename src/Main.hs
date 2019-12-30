module Main where

import Hashkell.Config     (execParser, commandLineOptions, CommandLineConfig(..))
import Hashkell.Types      (readMovie, Mode(SearchByHash))
import Hashkell.Network    (downloadSubtitles)

import Control.Monad            (forM_)
import Control.Concurrent.Async (forConcurrently_)
import Network.HTTP.Client      (defaultManagerSettings, newManager)

main :: IO ()
main = do
    commandLineConf <- execParser commandLineOptions
    manager <- newManager defaultManagerSettings

    let forStrategy = if sequential commandLineConf == Just True
                          then forM_
                          else forConcurrently_

    forStrategy (files commandLineConf) $ \fn -> do
        movie <- readMovie fn
        downloadSubtitles manager SearchByHash movie (languageCode commandLineConf)
