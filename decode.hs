import qualified Data.ByteString.Lazy as B
import System.Environment
import System.Exit
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits
import Data.Char
import Data.Functor
import Control.Monad
import System.Directory

import Codec.Container.Ogg.Page

oggTableOffset :: Get Word32
oggTableOffset = do
    skip 4
    getWord32le

oggTable :: Word32 -> Get [(Word32, Word32)]
oggTable offset = skip (fromIntegral offset) >> go
  where
    go = do
        ptr <- getWord32le
        if ptr /= 0 then do
            len <- getWord32le
            ((ptr, len) :) <$> go
        else return []


extract :: Word32 -> Word32 -> Get (B.ByteString)
extract off len = do
    skip (fromIntegral off)
    getLazyByteString (fromIntegral len)

getXor :: Word32 -> Get (Word8)
getXor off = do
    skip (fromIntegral off)
    present <- getWord8
    let wanted = 79 :: Word8
    return $ wanted `xor` present

magic :: B.ByteString
magic = B.pack $ map (fromIntegral . ord) "OggS"

decypher :: Word8 -> B.ByteString -> B.ByteString 
decypher x = B.map go
    where go 0 = 0
          go n | n == x    = n
               | otherwise = xor x n

checkOgg :: B.ByteString -> IO ()
checkOgg ogg = do
    let (tracks, pages, rest) = pageScan ogg
    let all_ok = all (checkPageCRC ogg) pages
    printf "    %d tracks, %3d pages, %d bytes remain. CRC ok? %s\n" (length tracks) (length pages) (B.length rest) (show all_ok)

checkPageCRC :: B.ByteString -> OggPage -> Bool
checkPageCRC ogg page =
    let raw_page = B.take (fromIntegral (pageLength page)) $
                   B.drop (fromIntegral (pageOffset page)) $ ogg
        raw_page' = pageWrite page
    in raw_page == raw_page'


main = do
    args <- getArgs
    file <- case args of
        [file] -> return file
        _ -> do
            prg <- getProgName
            putStrLn $ "Usage: " ++ prg ++ " <file.gme>"
            exitFailure
    bytes <- B.readFile file

    let oto = runGet oggTableOffset bytes
        ot = runGet (oggTable oto) bytes
        (oo,ol) = head ot
        ogg = runGet (extract oo ol) bytes
        x = runGet (getXor oo) bytes

    printf "Ogg table offset: %08X\n" oto
    printf "First Ogg table offset entry: %08X %d\n" oo ol
    printf "XOR value: %02X\n" x
    printf "First Ogg magic: %s\n" (show (B.take 4 ogg))
    printf "First Ogg magic xored: %s\n" (show (B.map (xor x) (B.take 4 ogg)))
    printf "Table entries: %d\n" (length ot)
    createDirectoryIfMissing False "oggs"
    forM_ ot $ \(oo,ol) -> do
        let rawogg = runGet (extract oo ol) bytes
        let filename = "oggs/" ++ file ++ printf "_%08x" oo ++ ".ogg"
        let ogg = decypher x rawogg
        B.writeFile filename ogg
        printf "Dumped decyphered ogg file to %s\n" filename
        when (x `B.elem` (B.take 58 rawogg)) $
            printf "Found XOR magic %02X in %s\n" x filename
        checkOgg ogg


