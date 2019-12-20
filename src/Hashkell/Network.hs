{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hashkell.Network ( downloadSubtitles
) where


import Network.HTTP.Conduit         (responseBody, http, Manager)
import Network.HTTP.Simple          (parseRequest, setRequestHeader, httpJSONEither, getResponseBody, HttpException)
import Conduit                      (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception            (catch)
import Data.Conduit.Binary          (sinkFileCautious)
import Data.Conduit.Zlib            (ungzip)
import Hashkell.Types               ( beautifulPrint
                                    , Movie(fileDirectory, fileHash, fileName, fileSize)
                                    , LanguageCode
                                    , Mode(SearchByHash, SearchByName)
                                    , QueryResult(..))

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

downloadSubtitles :: Manager -> Mode -> Movie -> LanguageCode -> IO ()
downloadSubtitles manager mode movie lang = do
    queryResults <- catch (queryForSubtitles mode movie lang) $ \(_ :: HttpException) -> do
        beautifulPrint movie "Warning: HtttpException querying for subtitle"
        return []
    let bestSubtitle = selectBestSubtitle queryResults
    case bestSubtitle of
        Nothing -> beautifulPrint movie "Didn't find a good subtitle candidate, skipping"
        Just q  -> do
            beautifulPrint movie $ "Downloading subtitle " ++ subtitlesLink q
            downloadQueryResult manager movie q

