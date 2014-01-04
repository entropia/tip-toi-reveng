import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits
import Data.List
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

hyp4 :: B.ByteString -> Bool
hyp4 b =
    if B.pack [0x00,0xF9,0xFF,0x01] `B.isPrefixOf` (B.drop 12 b)
    then b `B.index` 0 == 0x02
    else True

{- Not true
hyp5 :: B.ByteString -> Bool
hyp5 b = case parseLine b of
    Just l -> case l of
        Line _ _ (F2 _ n _ : cmds) -> case last cmds of
            A _ xs -> fromIntegral n == length xs
            _ -> False
        _ -> True
    Nothing -> True
-}

hyp6 :: B.ByteString -> Bool
hyp6 b = case parseLine b of
    Just l -> case l of
        Line _ _ cmds ->
            test cmds ||
            -- Special casing: For some files, we have extra 0x00 00 at the end of every line
            (if [Z,Z] `isSuffixOf` cmds then test (take (length cmds - 2) cmds) else False)
    Nothing -> True
  where non_empty_list (A _ (_:_)) = True
        non_empty_list (B _ (_:_)) = True
        non_empty_list (C (_:_)) = True
        non_empty_list _ = False
        test cmds = (last cmds == Z || non_empty_list (last cmds))
                    && all (not . non_empty_list) (init cmds)


hyps = [ -- (hyp1, "01 line length")
         (hyp2, "01 fixed prefix")
       , (hyp3, "00F9 FF01 at bytes 4-7")
       , (hyp4, "00F9 FF01 at bytes 12-15 only in 0200-lines")
       -- , (hyp5, "F2(_,n,_) indicates number of arguments to A(...)")
       , (hyp6, "0x00 or non-empty A,B,C terminate commands")
       ]

data Command
    = A Word16 [Word16]
    | B Word16 [Word16]
    | C [Word16]
    | D B.ByteString
    | E B.ByteString
    | F1 Word16 Word8
    | F2 Word16 Word8 Word16
    | G
    | Z
    deriving Eq

data Line = Line Word8 B.ByteString [Command]

prettyPrintLine :: Line -> String
prettyPrintLine (Line t b cs) = show t ++ extra ++ ": " ++ intercalate " " (map prettyPrintCommand cs)
  where extra | b == B.pack [0,0,0,0]  = ""
              | otherwise              = "[" ++ prettyHex b ++ "]"

prettyPrintCommand :: Command -> String
prettyPrintCommand (A n xs) = printf "A(%d,[%s])" n (intercalate "," (map (printf "%d") xs))
prettyPrintCommand (B n xs) = printf "B(%d,[%s])" n (intercalate "," (map (printf "%d") xs))
prettyPrintCommand (C xs) = printf "C([%s])" (intercalate "," (map (printf "%d") xs))
prettyPrintCommand (D b) = printf "D(%s)" (prettyHex b)
prettyPrintCommand (E b) = printf "D(%s)" (prettyHex b)
prettyPrintCommand (F1 n x) = printf "F1(%d,%d)" n x
prettyPrintCommand (F2 n x y) = printf "F2(%d,%d,%d)" n x y
prettyPrintCommand (G) = printf "G"
prettyPrintCommand (Z) = printf "0x00"

parseLine :: B.ByteString -> Maybe Line
parseLine = runGet begin
 where
    headers =
        [ (B.pack [0xE8,0xFF,0x01], format2 A)
        , (B.pack [0x00,0xFC,0x01], format2 B)
        , (B.pack [0xFF,0xFA,0x01,0xFF,0xFF], format2 (const C))
        , (B.pack [0x00,0xFD,0x01], skipFormat 3 D)
        , (B.pack [0xF0,0xFF,0x01], skipFormat 4 E)
        , (B.pack [0xF9,0xFF,0x01], formatF9)
        , (B.pack [0xFB,0xFF,0x01], skipFormat 6 (const G))
        , (B.pack [0x00], zero)
        ]
    -- Find the occurrence of a header
    begin = do
        done <- isEmpty
        if done
        then return Nothing
        else do
            tag <- getWord8
            b <- getLazyByteString 4
            cs <- getCmds
            return $ Just (Line tag b cs)

    getCmds = do
        done <- isEmpty
        if done then return [] else do

        r <- getRemainingLazyByteString
        case find (\(h,f) -> h `B.isPrefixOf` r) headers of
          Just (h,f) -> do
            c <- f
            cs <- getCmds
            return $ c:cs
          Nothing -> fail $ "unexpected command: " ++ prettyHex r

    skipFormat n con = do
        skip 3
        bs <- getLazyByteString n
        return $ con bs

    zero = do
        skip 1
        return Z

    formatF9 = do
        skip 3
        n <- getWord16le
        y <- getWord8
        if (y == 0)
        then do
            x <- getWord8
            return $ F1 n x
        else do
            x <- getWord16le
            return $ F2 n y x

    format2 con = do
        skip 3
        m <- getWord16le
        n <- getWord16le
        xs <- replicateM (fromIntegral n) getWord16le
        return $ con m xs


checkLine :: Int -> Line -> [String]
checkLine n_audio (Line _ _ cmds) =
    concatMap (checkCommand n_audio) cmds

checkCommand :: Int -> Command -> [String]
checkCommand n_audio c@(A _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in command " ++ prettyPrintCommand c
checkCommand n_audio c@(B _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in command " ++ prettyPrintCommand c
checkCommand n_audio c@(C xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in command " ++ prettyPrintCommand c
checkCommand n_audio c = []


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

prettyHex :: B.ByteString -> String
prettyHex = spaceout . map (printf "%02X") . B.unpack
  where spaceout (a:b:[]) = a ++ b
        spaceout (a:b:r) = a ++ b ++ " " ++ spaceout r
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
    printf "Invalid main table entries: %d\n " (length (filter (== 0xFFFFFFFF) mt))
    printf "Large main table entries: %d\n" (length (filter (> ato) mt))
    printf "First two entries: %08X %08X\n" (mt !! 0) (mt !! 1)

    let jtos = filter (< ato) $
               filter (/= 0xFFFFFFFF) (drop 2 mt)
    let jts = map (getJumpTable bytes) jtos

    printf "%d Jump tables follow:\n" (length jts)
    forM_ (zip jtos jts) $ \(o, jt) -> do
        printf "Jump table at %08X:\n" o
        forM_ jt $ \line -> case parseLine line of
            Nothing -> do
                printf "    --\n"
            Just l -> do
                -- printf "    %s\n" (prettyHex line)
                printf "    %s\n" (prettyPrintLine l)
                mapM_  (printf "     * %s\n") (checkLine (length at) l)

    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not. hyp) (concat jts)
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> case parseLine line of
                Nothing -> do
                    printf "    --\n"
                Just l -> do
                    printf "    %s\n" (prettyHex line)
                    printf "    %s\n" (prettyPrintLine l)

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


