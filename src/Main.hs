{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Exception (bracket)
import System.Environment(getArgs)
import System.IO(openBinaryFile,hClose,hFileSize,hSeek,IOMode(ReadMode),SeekMode(AbsoluteSeek,SeekFromEnd))
import qualified Data.ByteString.Lazy as L(hGet,unpack, append)
import Data.Binary.Get(runGet,getWord64le)
import Data.Binary.Put(runPut,putWord64le)
import Data.Word(Word64)
import Control.Monad(foldM)
import Data.Bits.Utils(w82s)
import Data.Hex(hex)

import Data.Digest.Pure.MD5(md5)

import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as S8


openSubtitlesUrl = "http://rest.opensubtitles.org/search"


downloadSubtitle url = do
    request' <- parseRequest $ "POST " ++ url
    let request
            = setRequestHeader "User-Agent" ["TemporaryUserAgent"]
            $ request'
    response <- httpJSON request :: IO (Response ())
    --print $ getResponseHeader "Content-Type" response
    print $ (getResponseBody response)

subDbHash fn = bracket (openBinaryFile fn ReadMode) hClose $ \h -> do
    let readSize = 64 * 1024

    hSeek h AbsoluteSeek 0
    begChunk <- L.hGet h readSize

    hSeek h SeekFromEnd (toInteger $ -readSize)
    endChunk <- L.hGet h readSize

    let totalChunk = L.append begChunk endChunk

    pure $ md5 totalChunk


  
main :: IO ()
main = do
  args <- getArgs
  let fn = head $ args
  hash <- subDbHash fn
  print hash
  --fileHash <- calculateHash fn
  --putStrLn $ "The hash of file " ++ fn ++ ": " ++ fileHash
  --downloadSubtitle openSubtitlesUrl

