{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hashkell.Types ( Movie (..)
                      , readMovie
                      , beautifulPrint
                      , LanguageCode
                      , Mode (SearchByHash, SearchByName)
                      , QueryResult(..)
) where

import Hashkell.Hash       (openSubtitlesHash)

import Control.Applicative (empty)
import Control.Exception   (catch, SomeException)
import Data.Aeson          (parseJSON, Value(Object), FromJSON, (.:))
import System.FilePath     (takeBaseName, takeExtension, takeDirectory)
import System.IO           (withBinaryFile, hFileSize, IOMode(ReadMode))

data Movie = Movie { fileName      :: String
                   , fileExtension :: String
                   , fileDirectory :: String
                   , fileSize      :: String
                   , fileHash      :: String
                   }

readMovie :: FilePath -> IO (Maybe Movie)
readMovie fp = catch go $ \(_ :: SomeException) -> handler
    where
        go = withBinaryFile fp ReadMode $ \h -> do
            hash <- openSubtitlesHash h
            fs <- hFileSize h
            return $ Just Movie { fileName      = takeBaseName fp
                                , fileExtension = takeExtension fp
                                , fileDirectory = takeDirectory fp
                                , fileSize      = show fs
                                , fileHash      = hash}
        handler = do
            putStrLn $ "[" ++ fp ++ "] " ++ "Couldn't read file, skipping..."
            return Nothing

beautifulPrint :: Movie -> String -> IO ()
beautifulPrint movie message = putStrLn $ "[" ++ fileName movie ++ fileExtension movie ++ "] " ++ message

data Mode = SearchByHash | SearchByName

type LanguageCode = String

data QueryResult = QueryResult { subFormat       :: String
                               , subDownloadLink :: String
                               , subtitlesLink   :: String
                               }

instance FromJSON QueryResult where
    parseJSON (Object v) = QueryResult <$> v .: "SubFormat" <*> v .: "SubDownloadLink" <*> v.: "SubtitlesLink"
    parseJSON _          = empty

