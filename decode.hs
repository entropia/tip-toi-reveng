import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import System.FilePath
import Data.Binary.Get
import Data.Word
import Text.Printf
import Data.Bits
import Data.List
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Ord
import Control.Monad
import System.Directory
import Numeric (showHex, readHex)
import qualified Data.Map as M

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

data Command
    = Play Word8
    | Random Word8 Word8
    | Cancel
    | Game Word8
    | Inc Word8 Word16
    | Set Word8 Word16
    | Unknown B.ByteString Word8 Word16
    deriving Eq

data Line = Line [Conditional] [Command] [Word16]

data Conditional
    = Eq  Word8 Word16
    | NEq Word8 Word16
    | Lt  Word8 Word16
    | GEq Word8 Word16
    | Unknowncond B.ByteString Word8 Word16

ppLine :: Line -> String
ppLine (Line cs as xs) = spaces (map ppConditional cs) ++ ": " ++ spaces (map ppCommand as) ++ media xs
  where media [] = ""
        media _  = " [" ++ commas (map show xs) ++ "]"


ppConditional :: Conditional -> String
ppConditional (Eq  g v)           = printf "$%d==%d?" g v
ppConditional (NEq g v)           = printf "$%d!=%d?" g v
ppConditional (Lt g v)            = printf "$%d< %d?" g v
ppConditional (GEq g v)           = printf "$%d>=%d?" g v
ppConditional (Unknowncond b g v) = printf "%d??%d? (%s)" g v (prettyHex b)

quote True = "'"
quote False= ""

ppCommand :: Command -> String
ppCommand (Play n)        = printf "P(%d)" n
ppCommand (Random a b)    = printf "P(%d-%d)" b a
ppCommand (Cancel)        = printf "C"
ppCommand (Game b)        = printf "G(%d)" b
ppCommand (Inc r n)       = printf "$%d+=%d" r n
ppCommand (Set r n)       = printf "$%d:=%d" r n
ppCommand (Unknown b r n) = printf "?($%d,%d) (%s)" r n (prettyHex b)

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
              Nothing -> return $ Unknowncond bytecode r n

        -- Actions
        b <- getWord8
        expectWord8 0
        cmds <- replicateM (fromIntegral b) $ do
            r <- getWord8
            expectWord8 0
            bytecode <- getLazyByteString 3
            case lookup bytecode actions of
              Just p -> p r
              Nothing -> do
                n <- getWord16le
                return $ Unknown bytecode r n

        -- Audio links
        n <- getWord16le
        xs <- replicateM (fromIntegral n) getWord16le
        return $ Line conds cmds xs

    expectWord8 n = do
        n' <- getWord8
        when (n /= n') $ do
            b <- bytesRead
            fail $ printf "At position 0x%08X, expected %d/%02X, got %d/%02X" (b-1) n n n' n'

    conditionals =
        [ (B.pack [0xF9,0xFF,0x01], Eq  )
        , (B.pack [0xFF,0xFF,0x01], NEq )
        , (B.pack [0xFB,0xFF,0x01], Lt )
        , (B.pack [0xFD,0xFF,0x01], GEq )
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

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

getAudioTable :: B.ByteString -> ([(Word32, Word32)], Bool)
getAudioTable bytes =
    let ato = runGet audioTableOffset bytes
        at = runGet (audioTable ato) bytes
    in case doubled at of Just at' -> (at', True)
                          Nothing  -> (at, False)

dumpAudioTo :: FilePath -> FilePath -> IO ()
dumpAudioTo directory file = do
    bytes <- B.readFile file

    -- Ogg file stuff
    let (at, at_doubled) = getAudioTable bytes
        x = runGet (getXor (fst (head at))) bytes

    printf "Audio Table entries: %d\n" (length at)

    createDirectoryIfMissing False directory
    forMn_ at $ \n (oo,ol) -> do
        let rawaudio = runGet (extract oo ol) bytes
        let audio = decypher x rawaudio
        let audiotype = fromMaybe "raw" $ lookup (B.take 4 audio) fileMagics
        let filename = printf "%s/%s_%04d.%s" directory (takeBaseName file) n audiotype
        if B.null audio
        then do
            printf "Skipping empty file %s...\n" filename
        else do
            B.writeFile filename audio
            printf "Dumped sample %d as %s\n" n filename

dumpScripts :: Maybe Int -> FilePath -> IO ()
dumpScripts sel file = do
    bytes <- B.readFile file
    let st = getScriptTable bytes
    let st' | Just n <- sel = filter ((== fromIntegral n) . fst) st
            | otherwise     = st

    forM_ st' $ \(i, ms) -> case ms of
        Nothing -> do
            printf "Script for OID %d: Disabled\n" i
        Just (o, lines) -> do
            printf "Script for OID %d: (at 0x%08X)\n" i o
            forM_ lines $ \(_, line) -> do
                printf "    %s\n" (ppLine line)
                -- mapM_  (printf "     * %s\n") (checkLine (length at) line)

dumpRawScripts :: FilePath -> IO ()
dumpRawScripts file = do
    bytes <- B.readFile file
    forM_ (getScriptTable bytes) $ \(i, ms) -> case ms of
        Nothing -> do
            printf "Script for OID %d: Disabled\n" i
        Just (o, lines) -> do
            printf "Script for OID %d: (at 0x%08X)\n" i o
            forM_ lines $ \(_, line) -> do
                printf "    %s\n" (prettyHex (runGet (extract o (lineLength line)) bytes))

dumpInfo :: FilePath -> IO ()
dumpInfo file = do
    bytes <- B.readFile file
    let st = getScriptTable bytes
        (at,at_doubled) = getAudioTable bytes
        x = runGet (getXor (fst (head at))) bytes

    printf "Scripts for OIDs from %d to %d; %d/%d are disabled.\n"
        (fst (head st)) (fst (last st))
        (length (filter (isNothing . snd) st)) (length st)
    printf "Magic XOR value: 0x%02X\n" x
    when at_doubled $ printf "Audio table repeated twice\n"
    printf "Audio Table entries: %d\n" (length at)

lint :: FilePath -> IO ()
lint file = do
    bytes <- B.readFile file
    let st = getScriptTable bytes
        (at,_) = getAudioTable bytes

    let hyps = [ (hyp1, "play indicies are correct")
               , (hyp2 (fromIntegral (length at)), "media indicies are correct")
               ]
    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not . hyp) (map snd (concatMap snd (mapMaybe snd st)))
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> do
                printf "    %s\n" (ppLine line)

    let segments = getSegments bytes
        overlapping_segments =
            filter (\((o1,l1,_),(o2,l2,_)) -> o1+l1 > o2) $
            zip segments (tail segments)
    unless (null overlapping_segments) $ do
        printf "Overlapping segments: %d\n"
            (length overlapping_segments)
        forM_ overlapping_segments $ \((o1,l1,d1),(o2,l2,d2)) ->
            printf "   Offset %08X Size %d (%s) overlaps Offset %08X Size %d (%s) by %d\n"
            o1 l1 d1 o2 l2 d2 (o1 + l1 - o2)
  where
    hyp1 :: Line -> Bool
    hyp1 (Line _ as mi) = all ok as
      where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
            ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                         0 <= b && b < fromIntegral (length mi)
            ok _ = True

    hyp2 :: Word16 -> Line -> Bool
    hyp2 n (Line _ _ mi) = all (<= n) mi


doubled :: Eq a => [a] -> Maybe [a]
doubled xs | take l2 xs == drop l2 xs = Just (take l2 xs)
           | otherwise                = Nothing
  where l = length xs
        l2 = l `div` 2



getSegments :: B.ByteString -> [(Word32, Word32, String)]
getSegments bytes =
    let ato = runGet audioTableOffset bytes
        (at, at_doubled) = getAudioTable bytes
        sto = runGet scriptTableOffset bytes
        st = getScriptTable bytes
    in sort $
            [ (0, 4, "Script table address") ] ++
            [ (4, 4, "Audio table address") ] ++
            [ (sto, fromIntegral (8 + 4 * length st), "Script table") ] ++
            [ (o, 2 + fromIntegral (length ls) * 4, "Script header for OID " ++ show i) |
                (i, Just (o, ls)) <- st
            ] ++
            [ (lo, lineLength l, printf "Script line Nr %d for OID %d" n i) |
                (i, Just (o, ls)) <- st,
                (n, (lo, l)) <- zip [1::Int ..] ls
            ] ++
            [ (ato, fromIntegral (8 * length at), "Audio table") ] ++
            [ (ato + fromIntegral (8 * length at), fromIntegral (8 * length at), "Duplicated audio table") |
              at_doubled
            ] ++
            [ (o, fromIntegral l, "Audio file " ++ show n ) | (n,(o,l)) <- zip [0..] at ]++
            [ (fromIntegral (B.length bytes), 0, "End of file") ]

printSegment (o,l,desc) = printf "At 0x%08X Size %8d: %s\n" o l desc

segments :: FilePath -> IO ()
segments file = do
    bytes <- B.readFile file
    mapM_ printSegment (getSegments bytes)

findPosition :: Integer -> FilePath -> IO ()
findPosition pos' file = do
    bytes <- B.readFile file
    let segments = getSegments bytes
    case find (\(o,l,_) -> pos >= o && pos < o + l) segments of
        Just s -> do
            printf "Offset 0x%08X is part of this segment:\n" pos
            printSegment s
        Nothing -> do
            let before = filter (\(o,l,_) -> pos >= o + l) segments
                after = filter (\(o,l,_) -> pos < o) segments
                printBefore | null before = printf "(nothing before)\n"
                            | otherwise   = printSegment (maximumBy (comparing (\(o,l,_) -> o+l)) before)
                printAfter  | null after  = printf "(nothing after)\n"
                            | otherwise   = printSegment (minimumBy (comparing (\(o,l,_) -> o)) after)
            printf "Offset %08X not found. It lies between these two segments:\n" pos
            printBefore
            printAfter

    where
    pos = fromIntegral pos'

unknown_segments :: FilePath -> IO ()
unknown_segments file = do
    bytes <- B.readFile file
    let segments = getSegments bytes
        unknown_segments =
            filter (\(o,l) -> not
                (l == 2 && runGet (skip (fromIntegral o) >> getWord16le) bytes == 0)) $
            filter (\(o,l) -> l > 0) $
            zipWith (\(o1,l1,_) (o2,_,_) -> (o1+l1, o2-(o1+l1)))
            segments (tail segments)
    printf "Unknown file segments: %d (%d bytes total)\n"
        (length unknown_segments) (sum (map snd unknown_segments))
    forM_ unknown_segments $ \(o,l) ->
        printf "   Offset: %08X to %08X (%d bytes)\n" o (o+l) l

forEachFile :: (FilePath -> IO ()) -> [FilePath] -> IO ()
forEachFile _ [] = main' []
forEachFile a [f] = a f 
forEachFile a fs = forM_ fs $ \f -> do 
    printf "%s:\n" f 
    a f

type State = M.Map Word8 Word16

initialState :: State
initialState = M.singleton 0 1

formatState :: State -> String
formatState s = spaces $ map (\(k,v) -> printf "$%d=%d" k v) $ M.toAscList s

play :: FilePath -> IO ()
play file = do
    bytes <- B.readFile file
    let st = getScriptTable bytes
    forEachNumber initialState $ \i s -> do
        case lookup (fromIntegral i) st of
            Nothing -> printf "OID %d not in main table\n" i >> return s
            Just Nothing -> printf "OID %d deactivated\n" i >> return s
            Just (Just (_,o_lines)) -> do
                let lines = map snd o_lines
                case find (enabledLine s) lines of
                    Nothing -> printf "None of these lines matched!\n" >> mapM_ (putStrLn . ppLine) lines >> return s
                    Just l -> do
                        printf "Executing:  %s\n" (ppLine l)
                        let s' = applyLine l s
                        printf "State now: %s\n" (formatState s')
                        return s'

enabledLine :: State -> Line -> Bool
enabledLine s (Line cond _ _) = all (condTrue s) cond

condTrue :: State -> Conditional -> Bool
condTrue s (Eq r n)  = s `value` r == n
condTrue s (NEq r n) = s `value` r /= n
condTrue s (Lt r n)  = s `value` r < n
condTrue s (GEq r n) = s `value` r >= n
condTrue _ _ = False

value :: State -> Word8 -> Word16
value m r = M.findWithDefault 0 r m

applyLine :: Line -> State -> State
applyLine (Line _ act _) s = foldl' go s act
  where go s (Set r n) = M.insert r n s
        go s (Inc r n) = M.insert r (s `value` r + n) s
        go s _         = s

forEachNumber :: s -> (Int -> s -> IO s) -> IO ()
forEachNumber state action = go state
  where
    go s = do
        putStrLn "Next OID touched? "
        str <- getLine
        case readMaybe str of
            Just i -> action i s >>= go
            Nothing -> do
                putStrLn "Not a number, please try again"
                go s


main' ("info": files)             = forEachFile dumpInfo files
main' ("media": "-d": dir: files) = forEachFile (dumpAudioTo dir) files
main' ("media": files)            = forEachFile (dumpAudioTo "media") files
main' ("scripts": files)          = forEachFile (dumpScripts Nothing) files
main' ("script":  file : n:[])
    | Just int <- readMaybe n     =             dumpScripts (Just int) file
main' ("raw-scripts": files)      = forEachFile dumpRawScripts files
main' ("lint": files)             = forEachFile lint files
main' ("segments": files)         = forEachFile segments files
main' ("segment": file : n :[])
    | Just int <- readMaybe n     =             findPosition int file
    | [(int,[])] <- readHex n     =             findPosition int file
main' ("holes": files)            = forEachFile unknown_segments files
main' ("play": file : [])         =             play file
main' _ = do
    prg <- getProgName
    putStrLn $ "Usage:"
    putStrLn $ prg ++ " info <file.gme>..."
    putStrLn $ "       general information"
    putStrLn $ prg ++ " media [-d dir] <file.gme>..."
    putStrLn $ "       dumps all audio samples to the given directory (default: media/)"
    putStrLn $ prg ++ " scripts <file.gme>..."
    putStrLn $ "       prints the decoded scripts for each OID"
    putStrLn $ prg ++ " script <file.gme> <n>"
    putStrLn $ "       prints the decoded scripts for the given OID"
    putStrLn $ prg ++ " raw-scripts <file.gme>..."
    putStrLn $ "       prints the scripts for each OID, in their raw form"
    putStrLn $ prg ++ " lint <file.gme>"
    putStrLn $ "       checks for errors in the file or in this program"
    putStrLn $ prg ++ " segments <file.gme>..."
    putStrLn $ "       lists all known parts of the file, with description."
    putStrLn $ prg ++ " segment <file.gme> <pos>"
    putStrLn $ "       which segment contains the given position."
    putStrLn $ prg ++ " holes <file.gme>..."
    putStrLn $ "       lists all unknown parts of the file."
    putStrLn $ prg ++ " play <file.gme>"
    putStrLn $ "       interactively play: Enter OIDs, and see what happens."
    exitFailure

main = getArgs >>= main'

