{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Exception        (bracket)
import           System.Environment       (getArgs)
import           System.IO                (withBinaryFile, hSeek, hFileSize, Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek, SeekFromEnd))
import           System.FilePath          (takeBaseName)
import qualified Data.ByteString.Lazy as B(hGet, unpack, append)
import           Data.Binary.Get          (runGet,getWord64le)
import           Data.Binary.Put          (runPut,putWord64le)
import           Data.Word                (Word64)
import           Control.Monad            (foldM)
import           Data.Bits.Utils          (w82s)
import           Data.Hex                 (hex)
import           Data.Digest.Pure.MD5     (md5, MD5Digest)
import           Network.HTTP.Simple      (parseRequest, setRequestHeader, httpJSON, getResponseBody, Response)
import qualified Data.ByteString.Char8 as S8


openSubtitlesUrl = "http://rest.opensubtitles.org/search"

data Movie = Movie { fileName :: String
                   , fileSize :: String
                   , hash :: String
                   } deriving Show

data Mode = SearchByHash | SearchByName

data Language = English | Portuguese | French

instance Show Language where
    show l = case l of
                  English    -> "eng"
                  Portuguese -> "pob"
                  French     -> "fre"

createRequestUrl :: Mode -> Movie -> Maybe Language -> String
createRequestUrl mode mov Nothing = case mode of
                                         SearchByHash -> openSubtitlesUrl ++ "/moviebytesize-" ++ fileSize mov
                                                                          ++ "/moviehash-"     ++ hash     mov
                                         SearchByName -> undefined
createRequestUrl mode mov (Just lang) = createRequestUrl mode mov Nothing ++ "/sublanguageid-" ++ show lang
                    
downloadSubtitle mode mov lang = do
    request' <- parseRequest (createRequestUrl mode mov lang)
    let request
            = setRequestHeader "User-Agent" ["TemporaryUserAgent"]
              request'
    response <- httpJSON request :: IO (Response ())
    --print $ getResponseHeader "Content-Type" response
    print $ getResponseBody response

shortSum :: Handle -> IO Word64
shortSum h = do
    fs <- hFileSize h
    hSeek h AbsoluteSeek 0 ; begin <- B.hGet h chunksize
    hSeek h SeekFromEnd (-(toInteger chunksize)) ; end <- B.hGet h chunksize
    return $ runGet (chunksum $ runGet (chunksum . fromInteger $ fs) end) begin
    where
        chunksize = 0x10000
        chunksum n = foldM (\a _ -> (+a) <$> getWord64le) n [1..(chunksize`div`8)]  

openSubtitlesHash :: Handle -> IO String
openSubtitlesHash h = do
    p <- shortSum h
    return (hex $ w82s $ reverse (B.unpack $ runPut $ putWord64le p))

foo :: FilePath -> IO Movie
foo fp = withBinaryFile fp ReadMode $ \h -> do
    hash <- openSubtitlesHash h
    fs <- hFileSize h
    return $ Movie {fileName = takeBaseName fp,fileSize = show fs, hash = hash}

main :: IO ()
main = do
  args <- getArgs
  let fn = head args
  movie <- foo fn
  print movie
  putStrLn $ "Hitting" ++ createRequestUrl SearchByHash movie (Just Portuguese)
  downloadSubtitle SearchByHash movie (Just Portuguese)

