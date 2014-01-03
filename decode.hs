import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits
import Data.Char
import Data.Functor
import Data.Maybe
import Control.Monad
import System.Directory
import Numeric (showHex)

--import Codec.Container.Ogg.Page

mainTableOffset :: Get Word32
mainTableOffset = do
    getWord32le

mainTable :: Word32 -> Get [Word32]
mainTable offset = do
    skip (fromIntegral offset)
    -- It seems that the third entry points to the end of the table
    until <- lookAhead $ do
        skip 4
        skip 4
        getWord32le
    let n_entries = fromIntegral ((until - offset) `div` 8)
    replicateM n_entries getWord32le

getJumpTable :: B.ByteString -> Word32 -> [B.ByteString]
getJumpTable bytes offset = 
    let mboffs = flip runGet bytes $ do
        skip (fromIntegral offset)
        -- Check if we have the pattern described in Table-Notes.md
        n <- getWord16le
        first <- lookAhead getWord32le
        if (offset + 2 + fromIntegral n * 4 == first) then do
            Just <$> replicateM (fromIntegral n) getWord32le
        else return Nothing
    in case mboffs of
        Just offs -> do
            -- Ignore the last one for now, until we know how it is terminated
            flip map (zip offs (tail offs ++ [last offs])) $ \(from, to) -> do
                runGet (extract from (to - from)) bytes
        Nothing -> []

-- Length correlation for 0100 jump table lines
hyp1 :: B.ByteString -> Bool
hyp1 b =
    if not (B.null b) && b `B.index` 0 == 0x01
    then B.length b == 16 + fromIntegral (b `B.index` 10) * 9
    else True

hyp2 :: B.ByteString -> Bool
hyp2 b =
    if not (B.null b) && b `B.index` 0 == 0x01
    then B.pack [0x01,0x00,0x00,0x00,0x00,0xF9,0xFF,0x01] `B.isPrefixOf` b
    else True

hyp3 :: B.ByteString -> Bool
hyp3 b =
    if not (B.null b)
    then B.pack [0x00,0xF9,0xFF,0x01] `B.isPrefixOf` (B.drop 4 b)
    else True

hyps = [ (hyp1, "01 line length")
       , (hyp2, "01 prefix")
       , (hyp3, "00F9 FF01 at bytes 4-7") ]


audioTableOffset :: Get Word32
audioTableOffset = do
    skip 4
    getWord32le

audioTable :: Word32 -> Get [(Word32, Word32, Int)]
audioTable offset = do
    skip (fromIntegral offset)
    until <- lookAhead getWord32le
    let n_entries = fromIntegral ((until - offset) `div` 8)
    sequence [ do
        ptr <- getWord32le
        len <- getWord32le
        return (ptr, len, n)
        | n <- [0..n_entries-1] ]

checkAT :: B.ByteString -> (Word32, Word32, Int) -> IO Bool
checkAT audio (off, len, n) =
    if fromIntegral off > B.length audio
    then do
        printf "    Entry %d: Offset %d > File size %d\n"
            n off (B.length audio)
        return False
    else if fromIntegral (off + len) > B.length audio
         then do
            printf "    Entry %d: Offset %d + Lengths %d > File size %d\n"
                n off len (B.length audio)
            return False
         else return True

extract :: Word32 -> Word32 -> Get (B.ByteString)
extract off len = do
    skip (fromIntegral off)
    getLazyByteString (fromIntegral len)

getXor :: Word32 -> Get (Word8)
getXor off = do
    skip (fromIntegral off)
    present <- getLazyByteString 4
    -- Brute force, but that's ok here
    return $ head $
        [ n
        | n <- [0..0xFF]
        , decypher n present `elem` map fst fileMagics
        ]

fileMagics :: [(B.ByteString, String)]
fileMagics =
    [ (BC.pack "RIFF", "wav")
    , (BC.pack "OggS", "ogg")]

decypher :: Word8 -> B.ByteString -> B.ByteString
decypher x = B.map go
    where go 0 = 0
          go 255 = 255
          go n | n == x    = n
               | n == xor x 255 = n
               | otherwise = xor x n

{-
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
-}

prettyPrint :: B.ByteString -> String
prettyPrint = spaceout . map (printf "%02X") . B.unpack
  where spaceout (a:b:r) = a ++ b ++ " " ++ spaceout r
        spaceout r = concat r

main = do
    args <- getArgs
    file <- case args of
        [file] -> return file
        _ -> do
            prg <- getProgName
            putStrLn $ "Usage: " ++ prg ++ " <file.gme>"
            exitFailure
    bytes <- B.readFile file

    -- Ogg file stuff
    let ato = runGet audioTableOffset bytes
        at = runGet (audioTable ato) bytes
        (oo,ol,_) = head at
        audio = runGet (extract oo ol) bytes
        x = runGet (getXor oo) bytes

    printf "Audio table offset: %08X\n" ato
    printf "First Audio table offset entry: %08X %d\n" oo ol
    printf "XOR value: %02X\n" x
    printf "First Audio magic: %s\n" (show (B.take 4 audio))
    printf "First Audio magic xored: %s\n" (show (B.map (xor x) (B.take 4 audio)))
    printf "Audio Table entries: %d\n" (length at)

    at_fixed <- filterM (checkAT bytes) at

    createDirectoryIfMissing False "oggs"
    forM_ at_fixed $ \(oo,ol,n) -> do
        let rawaudio = runGet (extract oo ol) bytes
        let audio = decypher x rawaudio
        let audiotype = fromMaybe "raw" $ lookup (B.take 4 audio) fileMagics
        let filename = printf "oggs/%s_%03d.%s" file n audiotype
        if B.null audio
        then do
            printf "File %s would be empty...\n" filename
        else do
            B.writeFile filename audio
            printf "Dumped decyphered %s\n" filename

            -- when (x `B.elem` (B.take 58 rawogg)) $
            --    printf "Found XOR magic %02X in %s\n" x filename

            -- checkOgg ogg

    -- Other stuff

    let mto = runGet mainTableOffset bytes
        mt  = runGet (mainTable mto) bytes
    printf "Main table offset: %08X\n" mto
    printf "Main table entries: %d\n" (length mt)
    printf "Invalid main table entries: %d\n" (length (filter (== 0xFFFFFFFF) mt))
    printf "First two entries: %08X %08X\n" (mt !! 0) (mt !! 1)

    let jtos = filter (/= 0xFFFFFFFF) (drop 2 mt)
    let jts = map (getJumpTable bytes) jtos

    printf "%d Jump tables follow:\n" (length jts)
    forM_ (zip jtos jts) $ \(o, jt) -> do
        printf "Jump table at %08X:\n" o
        forM_ jt $ \line ->
            printf "    %s\n" (prettyPrint line)

    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not. hyp) (concat jts)
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> printf "    %s\n" (prettyPrint line)

    let known_segments =
            [ (0, 4, "Main table address") ] ++
            [ (4, 4, "Media table address") ] ++
            [ (mto, fromIntegral (4 * length mt), "Main table") ] ++
            -- Add jump tables here. But how long are they?
            [ (ato, fromIntegral (8 * length at), "Media table") ] ++
            [ (o, fromIntegral l, "Media file " ++ show n ) | (o,l,n) <- at ]++
            [ (fromIntegral (B.length bytes), 0, "End of file") ]
    let unknown_segments =
            filter (\(o,l) -> l > 0) $
            zipWith (\(o1,l1,_) (o2,_,_) -> (o1+l1, o2-(o1+l1)))
            known_segments (tail known_segments)
    printf "Unknown file segments: %d (%d bytes total)\n"
        (length unknown_segments) (sum (map snd unknown_segments))
    forM_ unknown_segments $ \(o,l) ->
        printf "   Offset: %08X Size %d\n" o l


