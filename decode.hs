import qualified Data.ByteString.Lazy as B
import System.Environment
import System.Exit
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits

oggTableOffset :: Get Word32
oggTableOffset = do
    skip 512
    getWord32le

oggTable :: Word32 -> Get (Word32, Word32)
oggTable offset = do
    skip (fromIntegral offset)
    ptr <- getWord32le
    len <- getWord32le
    return (ptr, len)

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
        (oo,ol) = runGet (oggTable oto) bytes
        ogg = runGet (extract oo ol) bytes
        x = runGet (getXor oo) bytes

    printf "Ogg table offset: %08X\n" oto
    printf "Ogg table offset entry: %08X %d\n" oo ol
    printf "XOR value: %02X\n" x
    printf "Ogg magic: %s\n" (show (B.take 4 ogg))
    printf "Ogg magic xored: %s\n" (show (B.map (xor x) (B.take 4 ogg)))

