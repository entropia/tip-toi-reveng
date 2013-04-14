import qualified Data.ByteString as B
import System.Environment
import System.Exit
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits
import Data.List
import Data.Char
import Data.Ord

magic :: B.ByteString
magic = B.pack $ map (fromIntegral . ord) "OggS"

main = do
    args <- getArgs
    file <- case args of
        [file] -> return file
        _ -> do
            prg <- getProgName
            putStrLn $ "Usage: " ++ prg ++ " <file.gme>"
            exitFailure
    bytes <- B.readFile file

    let (x,pos) = maximumBy (comparing (length . snd)) [ 
                (x', B.findSubstrings xmagic bytes) |
                --x <- [0..255 :: Word8],
                x' <- [0xAD, 0x3B],
                let xmagic = B.map (xor x') magic
                ]
    printf "XOR: %02X\n" x

    putStrLn "Fundstellen:"
    mapM_ (printf "%08X\n") pos
