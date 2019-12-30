{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hashkell.Network ( downloadSubtitles
) where


import Hashkell.Config              (defaultLanguage)
import Hashkell.Types               ( beautifulPrint
                                    , Movie(..)
                                    , LanguageCode
                                    , Mode(SearchByHash, SearchByName)
                                    , QueryResult(..))

import Network.HTTP.Conduit         (responseBody, http, Manager)
import Network.HTTP.Simple          (parseRequest, setRequestHeader, httpJSONEither, getResponseBody)
import Conduit                      (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception            (catch, SomeException)
import Data.Conduit.Binary          (sinkFileCautious)
import Data.Conduit.Zlib            (ungzip)

openSubtitlesUrl :: String
openSubtitlesUrl = "http://rest.opensubtitles.org/search"

createRequestUrl :: Mode -> Movie -> LanguageCode -> String
createRequestUrl mode mov lang = case mode of
                                      SearchByHash -> openSubtitlesUrl ++ "/moviebytesize-" ++ fileSize mov
                                                                       ++ "/moviehash-"     ++ fileHash mov
                                                                       ++ "/sublanguageid-"  ++ lang
                                      SearchByName -> undefined
                    
queryForSubtitles :: Mode -> Movie -> LanguageCode -> IO [QueryResult]
queryForSubtitles mode mov lang = do
    request' <- parseRequest (createRequestUrl mode mov lang)
    let request
            = setRequestHeader "User-Agent" ["TemporaryUserAgent"]
              request'
    results <- httpJSONEither request
    case getResponseBody results of
        Left  _            -> return []
        Right queryResults -> return queryResults

-- Since open subtitles orders by score, the first result should be the best one
selectBestSubtitle :: [QueryResult] -> Maybe QueryResult
selectBestSubtitle []     = Nothing
selectBestSubtitle (x:_) = Just x

-- "uses interleaved IO to write the response body to a file in constant memory space."
--  this also decompresses the gzip while downloading the file
downloadQueryResult :: Manager -> Movie -> QueryResult -> IO ()
downloadQueryResult manager movie q = do
    request <- parseRequest $ subDownloadLink q
    runResourceT $ do
        response <- http request manager
        let saveLocation = fileDirectory movie ++ '/':fileName movie ++ '.':subFormat q
        runConduit $ responseBody response .| ungzip .| sinkFileCautious saveLocation

downloadSubtitles :: Manager -> Mode -> Maybe Movie -> Maybe LanguageCode -> IO ()
downloadSubtitles _ _ Nothing _ = return ()
downloadSubtitles manager mode movie Nothing = do
    lang <- defaultLanguage
    downloadSubtitles manager mode movie (Just lang)
downloadSubtitles manager mode (Just movie) (Just lang) = do
    queryResults <- catch (queryForSubtitles mode movie lang) $ \(_ :: SomeException) -> do
        beautifulPrint movie "Error: Couldn't query for subtitle, skipping..."
        return []
    let bestSubtitle = selectBestSubtitle queryResults
    case bestSubtitle of
        Nothing -> beautifulPrint movie "Didn't find a good subtitle candidate, skipping"
        Just q  -> do
            beautifulPrint movie $ "Downloading subtitle " ++ subtitlesLink q
            catch (downloadQueryResult manager movie q) $ \(_ :: SomeException) -> 
                beautifulPrint movie "Error: Couldn't download subtitle, skipping... "

