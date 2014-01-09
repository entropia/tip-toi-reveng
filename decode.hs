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

scriptTableOffset :: Get Word32
scriptTableOffset = do
    getWord32le

scriptTableParser :: Word32 -> Get [(Word16, Word32)]
scriptTableParser offset = do
    skip (fromIntegral offset)
    last_code <- getWord16le
    0 <- getWord16le
    first_code <- getWord16le
    0 <- getWord16le

    offs <- replicateM (fromIntegral (last_code - first_code + 1)) $ do
        getWord32le
    return $ zip [first_code .. last_code] offs

parseLine :: B.ByteString -> Line
parseLine = runGet lineParser

getScriptTable :: B.ByteString -> [(Word16, Maybe (Word32, [(Word32, Line)]))]
getScriptTable bytes =
    let sto = runGet scriptTableOffset bytes
    in  map getScript $ runGet (scriptTableParser sto) bytes
  where
    getScript (i, 0xFFFFFFFF)
        = (i, Nothing)
    getScript (i, offset)
        = (i, Just (offset, map getLine (getScriptOffsets offset)))

    getScriptOffsets offset = flip runGet bytes $ do
        skip (fromIntegral offset)
        n <- getWord16le
        replicateM (fromIntegral n) getWord32le

    getLine offset
        = (offset, runGet (skip (fromIntegral offset) >> lineParser) bytes)

hyp3 :: Line -> Bool
hyp3 (Line _ as mi) = all ok as
  where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
        ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                     0 <= b && b < fromIntegral (length mi)
        ok _ = True

hyps = [ -- (hyp2, "01 fixed prefix")
         (hyp3, "play indicies are correct")
       ]

data Command
    = Play Word8
    | Random Word8 Word8
    | Cancel
    | Game Word8
    | Inc Word8 Word16
    | Set Word8 Word16
    deriving Eq

data Line = Line [Conditional] [Command] [Word16]

data Conditional
    = Eq Word8 Word16
    | NEq Word8 Word16

ppLine :: Line -> String
ppLine (Line cs as xs) = spaces (map ppConditional cs) ++ ": " ++ spaces (map ppCommand as) ++ media xs
  where media [] = ""
        media _  = " [" ++ commas (map show xs) ++ "]"


ppConditional :: Conditional -> String
ppConditional (Eq  g v) = printf "$%d==%d?" g v
ppConditional (NEq g v) = printf "$%d!=%d?" g v

quote True = "'"
quote False= ""

ppCommand :: Command -> String
ppCommand (Play n)     = printf "P(%d)" n
ppCommand (Random a b) = printf "P(%d-%d)" b a
ppCommand (Cancel)     = printf "C"
ppCommand (Game b)     = printf "G(%d)" b
ppCommand (Inc r n)    = printf "$%d+=%d" r n
ppCommand (Set r n)    = printf "$%d:=%d" r n

spaces = intercalate " "
commas = intercalate ","

lineLength :: Line -> Word32
lineLength (Line conds cmds audio) = fromIntegral $
    2 + 8 * length conds + 2 + 7 * length cmds + 2 + 2 * length audio

lineParser :: Get Line
lineParser = begin
 where
   -- Find the occurrence of a header
    begin = do
        -- Conditionals
        a <- getWord8
        expectWord8 0
        conds <- replicateM (fromIntegral a) $ do
            expectWord8 0
            r <- getWord8
            expectWord8 0
            bytecode <- getLazyByteString 3
            n <- getWord16le
            case lookup bytecode conditionals of
              Just p -> return $ p r n
              Nothing -> fail $ "Unknown conditional: " ++ prettyHex bytecode

        -- Actions
        b <- getWord8
        expectWord8 0
        cmds <- replicateM (fromIntegral b) $ do
            r <- getWord8
            expectWord8 0
            bytecode <- getLazyByteString 3
            case lookup bytecode actions of
              Just p -> p r
              Nothing -> fail $ "Unknown command: " ++ prettyHex bytecode

        -- Audio links
        n <- getWord16le
        xs <- replicateM (fromIntegral n) getWord16le
        return $ Line conds cmds xs

    expectWord8 n = do
        n' <- getWord8
        when (n /= n') $ do
            b <- bytesRead
            fail $ printf "At position %0X, expected %d/%02X, got %d/%02X" (b-1) n n n' n'

    conditionals =
        [ (B.pack [0xF9,0xFF,0x01], Eq  )
        , (B.pack [0xFB,0xFF,0x01], NEq )
        ]

    actions =
        [ (B.pack [0xE8,0xFF,0x01], \r -> do
            unless (r == 0) $ fail "Non-zero register for Play command"
            a <- getWord8
            expectWord8 0
            return (Play a))
        , (B.pack [0x00,0xFC,0x01], \r -> do
            unless (r == 0) $ fail "Non-zero register for Random command"
            a <- getWord8
            b <- getWord8
            return (Random a b))
        , (B.pack [0xFF,0xFA,0x01], \r -> do
            unless (r == 0) $ fail "Non-zero register for Cancel command"
            expectWord8 0xFF
            expectWord8 0xFF
            return Cancel)
        , (B.pack [0x00,0xFD,0x01], \r -> do
            unless (r == 0) $ fail "Non-zero register for Game command"
            a <- getWord8
            expectWord8 0
            return (Game a))
        , (B.pack [0xF0,0xFF,0x01], \r -> do
            n <- getWord16le
            return (Inc r n))
        , (B.pack [0xF9,0xFF,0x01], \r -> do
            n <- getWord16le
            return (Set r n))
        ]
 


checkLine :: Int -> Line -> [String]
checkLine n_audio l@(Line _ _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in line " ++ ppLine l
checkLine n_audio _ = []

audioTableOffset :: Get Word32
audioTableOffset = do
    skip 4
    getWord32le

audioTable :: Word32 -> Get [(Word32, Word32)]
audioTable offset = do
    skip (fromIntegral offset)
    until <- lookAhead getWord32le
    let n_entries = fromIntegral ((until - offset) `div` 8)
    replicateM n_entries $ do
        ptr <- getWord32le
        len <- getWord32le
        return (ptr, len)

checkAT :: B.ByteString -> Int -> (Word32, Word32) -> IO Bool
checkAT bytes n (off, len) =
    if fromIntegral off > B.length bytes
    then do
        printf "    Entry %d: Offset %d > File size %d\n"
            n off (B.length bytes)
        return False
    else if fromIntegral (off + len) > B.length bytes
         then do
            printf "    Entry %d: Offset %d + Lengths %d > File size %d\n"
                n off len (B.length bytes)
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

prettyHex :: B.ByteString -> String
prettyHex = spaceout . map (printf "%02X") . B.unpack
  where spaceout (a:b:[]) = a ++ b
        spaceout (a:b:r) = a ++ b ++ " " ++ spaceout r
        spaceout r = concat r

forMn_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
forMn_ l f = forM_ (zip l [0..]) $ \(x,n) -> f n x

forMn :: Monad m => [a] -> (Int -> a -> m b) -> m [b]
forMn l f = forM (zip l [0..]) $ \(x,n) -> f n x

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
        (oo,ol) = head at
        audio = runGet (extract oo ol) bytes
        x = runGet (getXor oo) bytes


    printf "Filename %s\n" file
    printf "File size: %08X (%d)\n" (B.length bytes) (B.length bytes)
    printf "Checksum found %08X, calculated %08X\n"
        (runGet (skip (fromIntegral (B.length bytes) - 4) >> getWord32le) bytes)
        (foldl' (+) 0 (map fromIntegral (B.unpack (B.take (B.length bytes -4) bytes))) :: Word32)
    printf "Audio table offset: %08X\n" ato
    printf "First Audio table offset entry: %08X %d\n" oo ol
    printf "XOR value: %02X\n" x
    printf "First Audio magic: %s\n" (show (B.take 4 audio))
    printf "First Audio magic xored: %s\n" (show (B.map (xor x) (B.take 4 audio)))

    (at, at_doubled) <-
        if take (length at `div` 2) at == drop (length at `div` 2) at
        then do
            printf "Audio table repeats itself! Ignoring first half.\n"
            return (take (length at `div` 2) at, True)
        else
            return (at, False)

    printf "Audio Table entries: %d\n" (length at)

    forMn_ at (checkAT bytes)

    createDirectoryIfMissing False "oggs"
    forMn_ at $ \n (oo,ol) -> do
        let rawaudio = runGet (extract oo ol) bytes
        let audio = decypher x rawaudio
        let audiotype = fromMaybe "raw" $ lookup (B.take 4 audio) fileMagics
        let filename = printf "oggs/%s_%04d.%s" file n audiotype
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

    let sto = runGet scriptTableOffset bytes
    let st = getScriptTable bytes
    printf "Script table offset: 0x%08X\n" sto
    printf "Script table entries: %d\n" (length st)
    printf "Disabled entries: %d\n" (length (filter (isNothing . snd) st))

    forM_ st $ \(i, ms) -> case ms of
        Nothing -> do
            printf "Script for OID %d: Disabled\n" i
        Just (o, lines) -> do
            printf "Script for OID %d: (at 0x%08X)\n" i o
            forM_ lines $ \(_, line) -> do
                printf "    %s\n" (ppLine line)
                mapM_  (printf "     * %s\n") (checkLine (length at) line)

    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not . hyp) (map snd (concatMap snd (mapMaybe snd st)))
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> do
                printf "    %s\n" (ppLine line)

    let known_segments = sort $
            [ (0, 4, "Script table address") ] ++
            [ (4, 4, "Audio table address") ] ++
            [ (sto, fromIntegral (8 + 4 * length st), "Script table") ] ++
            [ (o, 2 + fromIntegral (length ls) * 4, "Script header for OID " ++ show i) |
                (i, Just (o, ls)) <- st
            ] ++
            [ (lo, lineLength l, "Script line for OID " ++ show i) |
                (i, Just (o, ls)) <- st,
                (lo, l) <- ls
            ] ++
            [ (ato, fromIntegral (8 * length at), "Audio table") ] ++
            [ (ato + fromIntegral (8 * length at), fromIntegral (8 * length at), "Duplicated audio table") |
              at_doubled
            ] ++
            [ (o, fromIntegral l, "Audio file " ++ show n ) | (n,(o,l)) <- zip [0..] at ]++
            [ (fromIntegral (B.length bytes), 0, "End of file") ]
    let overlapping_segments =
            filter (\((o1,l1,_),(o2,l2,_)) -> o1+l1 > o2) $
            zip known_segments (tail known_segments)
    printf "Overlapping segments: %d\n"
        (length overlapping_segments)
    forM_ overlapping_segments $ \((o1,l1,d1),(o2,l2,d2)) ->
        printf "   Offset %08X Size %d (%s) overlaps Offset %08X Size %d (%s) by %d\n"
            o1 l1 d1 o2 l2 d2 (o1 + l1 - o2)
    let unknown_segments =
            filter (\(o,l) -> not
                (l == 2 && runGet (skip (fromIntegral o) >> getWord16le) bytes == 0)) $
            filter (\(o,l) -> l > 0) $
            zipWith (\(o1,l1,_) (o2,_,_) -> (o1+l1, o2-(o1+l1)))
            known_segments (tail known_segments)
    printf "Unknown file segments: %d (%d bytes total)\n"
        (length unknown_segments) (sum (map snd unknown_segments))
    forM_ unknown_segments $ \(o,l) ->
        printf "   Offset: %08X to %08X (%d bytes)\n" o (o+l) l


