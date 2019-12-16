{-# LANGUAGE OverloadedStrings #-}

module Hashkell.Types ( Movie (fileDirectory, fileHash, fileName, fileSize)
                      , readMovie
                      , beautifulPrint
                      , Language(..)
                      , Mode (SearchByHash, SearchByName)
                      , QueryResult(..)
) where

import Control.Applicative (empty)
import Data.Aeson          (parseJSON, Value(Object), FromJSON, (.:))
import System.FilePath     (takeBaseName, takeDirectory)
import System.IO           (withBinaryFile, hFileSize, IOMode(ReadMode))
import Hashkell.Hash       (openSubtitlesHash)

data Movie = Movie { fileName      :: String
                   , fileDirectory :: String
                   , fileSize      :: String
                   , fileHash      :: String
                   } deriving Show

readMovie :: FilePath -> IO Movie
readMovie fp = withBinaryFile fp ReadMode $ \h -> do
    hash <- openSubtitlesHash h
    fs <- hFileSize h
    return $ Movie {fileName = takeBaseName fp, fileDirectory = takeDirectory fp, fileSize = show fs, fileHash = hash}

beautifulPrint :: Movie -> String -> IO ()
beautifulPrint movie message = putStrLn $ "[" ++ fileName movie ++ "] " ++ message


data Mode = SearchByHash | SearchByName

data Language = English | Portuguese | French

instance Show Language where
    show l = case l of
                  English    -> "eng"
                  Portuguese -> "pob"
                  French     -> "fre"

data QueryResult = QueryResult { subFormat       :: String
                               , subDownloadLink :: String
                               , subtitlesLink   :: String
                               } deriving Show

instance FromJSON QueryResult where
    parseJSON (Object v) = QueryResult <$> v .: "SubFormat" <*> v .: "SubDownloadLink" <*> v.: "SubtitlesLink"
    parseJSON _          = empty

