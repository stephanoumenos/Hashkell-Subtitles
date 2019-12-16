module Hashkell.Hash ( openSubtitlesHash
) where


import           Control.Monad            (foldM)
import           Data.Binary.Get          (runGet,getWord64le)
import           Data.Binary.Put          (runPut,putWord64le)
import           Data.Bits.Utils          (w82s)
import qualified Data.ByteString.Lazy as B(hGet, unpack, append)
import           Data.Hex                 (hex)
import           Data.Word                (Word64)
import           System.IO                (hFileSize, hSeek, Handle, SeekMode(AbsoluteSeek, SeekFromEnd))

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

