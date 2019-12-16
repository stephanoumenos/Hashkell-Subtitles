module Main where

import Hashkell.Types     (readMovie, Mode(SearchByHash), Language(Portuguese))
import Hashkell.Network   (downloadSubtitles)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let fn = head args
  movie <- readMovie fn
  downloadSubtitles SearchByHash movie (Just Portuguese)

