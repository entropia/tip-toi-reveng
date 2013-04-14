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


main = do
    args <- getArgs
    file <- case args of
        [file] -> return file
        _ -> do
            prg <- getProgName
            putStrLn $ "Usage: " ++ prg ++ " <file.gme>"
            exitFailure
    bytes <- B.readFile file

    let magic = B.pack $ map (fromIntegral . ord) "OggS"

    let stats = {- sortBy (comparing snd) -} [ 
            (x, length $ B.findSubstrings xmagic bytes) |
            x <- [0..255 :: Word8],
            let xmagic = B.map (xor x) magic
            ]
    mapM (uncurry (printf "%02X: %d\n")) stats
