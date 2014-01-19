{-# LANGUAGE GeneralizedNewtypeDeriving, RecursiveDo, ScopedTypeVariables, GADTs #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Binary.Builder as Br
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
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Arrow (second)
import Data.Time
import System.Locale

-- Main data types
data Conditional
    = Eq  Word8 Word16
    | NEq Word8 Word16
    | Lt  Word8 Word16
    | GEq Word8 Word16
    | Unknowncond B.ByteString Word8 Word16

data Command
    = Play Word8
    | Random Word8 Word8
    | Cancel
    | Game Word8
    | Inc Word8 Word16
    | Set Word8 Word16
    | Unknown B.ByteString Word8 Word16
    deriving Eq

type PlayList = [Word16]

data Line = Line Offset [Conditional] [Command] PlayList

data TipToiFile = TipToiFile
    { ttProductId :: Word32
    , ttRawXor :: Word32
    , ttComment :: B.ByteString
    , ttDate :: B.ByteString
    , ttInitialRegs :: [Word16]
    , ttScripts :: [(Word16, Maybe [Line])]
    , ttAudioFiles :: [B.ByteString]
    , ttAudioFilesDoubles :: Bool
    , ttAudioXor :: Word8
    , ttChecksum :: Word32
    , ttChecksumCalc :: Word32
    }


-- Assembling .gme files

-- Assembly monad
-- We need a data structure that we can extract its length from before we know its values
-- So we will use a lazy pair of length (Int) and builder

newtype SPutM a = SPutM (StateT Word32 (Writer Br.Builder) a) deriving (Functor, Monad, MonadFix)
type SPut = SPutM ()

putWord8 :: Word8 -> SPut
putWord8 w = SPutM (tell (Br.singleton w) >> modify (+1))

putWord16 :: Word16 -> SPut
putWord16 w = SPutM (tell (Br.putWord16le w) >> modify (+2))

putWord32 :: Word32 -> SPut
putWord32 w = SPutM (tell (Br.putWord32le w) >> modify (+4))

putBS :: B.ByteString -> SPut
putBS bs = SPutM (tell (Br.fromLazyByteString bs) >> modify (+ fromIntegral (B.length bs)))

putArray :: Integral n => (n -> SPut) -> [SPut] -> SPut
putArray h xs = do
    h (fromIntegral (length xs))
    sequence_ xs

data FunSplit m where
    FunSplit :: forall m a . (a -> m ()) -> m a -> FunSplit m


mapFstMapSnd :: forall m. MonadFix m => [FunSplit m] -> m ()
mapFstMapSnd xs = const () `liftM` go xs (return ())
  where
    go :: [FunSplit m] -> m b -> m b
    go [] cont = cont
    go (FunSplit f s:xs) cont = mdo
        f v
        (v,vs) <- go xs $ do
            vs <- cont
            v <- s
            return (v,vs)
        return vs

offsetsAndThen :: [SPut] -> SPut
offsetsAndThen = mapFstMapSnd . map go
    where go x = FunSplit putWord32 (getAddress x)

putOffsets :: Integral n => (n -> SPut) -> [SPut] -> SPut
putOffsets h xs = mdo
    h (fromIntegral (length xs))
    offsetsAndThen xs

seek :: Word32 -> SPut
seek to = SPutM $ do
    now <- get
    when (now > to) $ do
        fail $ printf "Cannot seek to 0x%08X, already at 0x%08X" to now
    tell $ (Br.fromLazyByteString (B.replicate (fromIntegral (to-now)) 0))
    modify (+ (to-now))

-- Puts something, returning the offset to the beginning of it.
getAddress :: SPut -> SPutM Word32
getAddress (SPutM what) = SPutM $ do
    a <- get
    what
    return a

runSPut :: SPut -> B.ByteString
--runSPut (SPutM act) = Br.toLazyByteString $ evalState (execWriterT act) 0
runSPut (SPutM act) = Br.toLazyByteString $ execWriter (evalStateT act 0)


putTipToiFile :: TipToiFile -> SPut
putTipToiFile tt = mdo
    putWord32 sto
    putWord32 mft
    putWord32 0x238b
    putWord32 ast -- Additional script table
    putWord32 gto -- Game table offset
    putWord32 (ttProductId tt)
    putWord32 iro
    putWord32 (ttRawXor tt)
    putWord8 (fromIntegral (B.length (ttComment tt)))
    putBS (ttComment tt)
    putBS (ttDate tt)
    seek 0x200 -- Just to be safe
    sto <- getAddress $ putScriptTable (ttScripts tt)
    ast <- getAddress $ putWord16 0x00 -- For now, no additional script table
    gto <- getAddress $ putGameTable
    iro <- getAddress $ putInitialRegs (ttInitialRegs tt)
    mft <- getAddress $ putAudioTable (ttAudioXor tt) (ttAudioFiles tt)
    return ()

putGameTable :: SPut
putGameTable = putWord32 0 -- Stub

putScriptTable :: [(Word16, Maybe [Line])] -> SPut
putScriptTable [] = error "Cannot create file with an empty script table"
putScriptTable scripts = mdo
    putWord32 (fromIntegral last)
    putWord32 (fromIntegral first)
    mapFstMapSnd (map go [first .. last])
    return ()
  where
    go i = case M.lookup i m of
        Just (Just l) -> FunSplit putWord32 (getAddress $ putLines l)
        _ -> FunSplit (\_ -> putWord32 0xFFFFFFFF) (return ())
    m = M.fromList scripts
    first = fst (M.findMin m)
    last = fst (M.findMax m)

putInitialRegs :: [Word16] -> SPut
putInitialRegs = putArray putWord16 . map putWord16

putLines :: [Line] -> SPut
putLines = putOffsets putWord16 . map putLine

putLine :: Line -> SPut
putLine (Line _ conds acts idx) = do
    putArray putWord16 $ map putCond conds
    putArray putWord16 $ map putCommand acts
    putArray putWord16 $ map putWord16 idx

putCond :: Conditional -> SPut
putCond (Eq r v) = do
    putWord8 0
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xF9, 0xFF, 0x01]
    putWord16 v
putCond (Lt r v) = do
    putWord8 0
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xFB, 0xFF, 0x01]
    putWord16 v
putCond (GEq r v) = do
    putWord8 0
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xFD, 0xFF, 0x01]
    putWord16 v
putCond (NEq r v) = do
    putWord8 0
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xFF, 0xFF, 0x01]
    putWord16 v
putCond (Unknowncond b r v) = do
    putWord8 0
    putWord8 r
    putWord8 0
    putBS b
    putWord16 v

putCommand :: Command -> SPut
putCommand (Set r v) = do
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xF9, 0xFF, 0x01]
    putWord16 v
putCommand (Inc r v) = do
    putWord8 r
    putWord8 0
    mapM_ putWord8 [0xF0, 0xFF, 0x01]
    putWord16 v
putCommand (Play n) = do
    putWord16 0
    mapM_ putWord8 [0xE8, 0xFF, 0x01]
    putWord8 n
    putWord8 0
putCommand (Random a b) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFC, 0x01]
    putWord8 a
    putWord8 b
putCommand (Game n) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFD, 0x01]
    putWord8 n
    putWord8 0
putCommand Cancel = do
    putWord16 0
    mapM_ putWord8 [0xFF, 0xFA, 0x01]
    putWord16 0xFFFF
putCommand (Unknown b r v) = do
    putWord8 r
    putWord8 0
    putBS b
    putWord16 v

putAudioTable :: Word8 -> [B.ByteString] -> SPut
putAudioTable x as = mapFstMapSnd
    [ FunSplit (\o -> putWord32 o >> putWord32 (fromIntegral (B.length a)))
               (getAddress (putBS (decypher x a)))
    | a <- as ]

-- Reverse Engineering Monad

type Offset = Word32
type Segment = (Offset, Word32, String)
type Segments = [Segment]

newtype SGet a = SGet (ReaderT B.ByteString (Writer [Segment]) a)
    deriving (Functor, Monad)

getAt :: Offset -> (Get a) -> SGet a
getAt offset act = SGet $ runGet (skip (fromIntegral offset) >> act) <$> ask

getSegAt :: Offset -> String -> (Get a) -> SGet a
getSegAt offset desc act = SGet $ do
    bytes <- ask
    let (a, _, i) = runGetState  (skip (fromIntegral offset) >> act) bytes 0
    tell [(offset, fromIntegral i - offset, desc)]
    return a

getLength :: SGet Word32
getLength = fromIntegral . B.length <$> getAllBytes

getAllBytes :: SGet B.ByteString
getAllBytes = SGet ask

runSGet :: SGet a -> B.ByteString -> (a, Segments)
runSGet (SGet act) bytes =
    second (sort . ((fromIntegral (B.length bytes), 0, "End of file"):)) $
    runWriter $ runReaderT act bytes

-- Parsers

getScriptTableOffset :: SGet Offset
getScriptTableOffset = getSegAt 0x00 "Script Table Offset" getWord32le

getScriptTable :: Offset -> SGet [(Word16, Offset)]
getScriptTable offset = getSegAt offset "Script table" $ do
    last_code <- getWord16le
    0 <- getWord16le
    first_code <- getWord16le
    0 <- getWord16le

    offs <- replicateM (fromIntegral (last_code - first_code + 1)) $ do
        getWord32le
    return $ zip [first_code .. last_code] offs

getScripts :: SGet [(Word16, Maybe [Line])]
getScripts = do
    sto <- getScriptTableOffset
    sto <- getScriptTable sto
    mapM getScript sto
  where
    getScript (i, 0xFFFFFFFF) = return (i, Nothing)
    getScript (i, offset)     = do
        los <- getLineOffsets i offset
        ls <- forMn los $ getLine i
        return (i, Just ls)

    getLineOffsets i offset = getSegAt offset (printf "Script header for OID %d" i) $ do
        array getWord16le getWord32le

    getLine i n offset = getSegAt offset (printf "Script Line %d for OID %d" n i) $
        lineParser offset

lineParser :: Offset -> Get Line
lineParser offset = begin
 where
   -- Find the occurrence of a header
    begin = do
        -- Conditionals
        conds <- array getWord16le $ do
            expectWord8 0
            r <- getWord8
            expectWord8 0
            bytecode <- getLazyByteString 3
            n <- getWord16le
            case lookup bytecode conditionals of
              Just p -> return $ p r n
              Nothing -> return $ Unknowncond bytecode r n

        -- Actions
        cmds <- array getWord16le $ do
            r <- getWord8
            expectWord8 0
            bytecode <- getLazyByteString 3
            case lookup bytecode actions of
              Just p -> p r
              Nothing -> do
                n <- getWord16le
                return $ Unknown bytecode r n

        -- Audio links
        xs <- array getWord16le getWord16le
        return $ Line offset conds cmds xs

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

getAudioTable :: Offset -> SGet [(Word32, Word32)]
getAudioTable offset = getSegAt offset "Audio Table" $ do
    until <- lookAhead getWord32le
    let n_entries = fromIntegral ((until - offset) `div` 8)
    replicateM n_entries $ do
        ptr <- getWord32le
        len <- getWord32le
        return (ptr, len)

getAudioTableOffset :: SGet Offset
getAudioTableOffset = getSegAt 0x4 "Audio table offset" getWord32le

getAudios :: SGet ([B.ByteString], Bool, Word8)
getAudios = do
    ato <- getAudioTableOffset
    at <- getAudioTable ato
    when (null at) $ fail "No audio files found, aborting"
    let (at', at_doubled) | Just at' <- doubled at = (at', True)
                          | otherwise              = (at, False)
    x <- getAt (fst (head at')) $ getXor
    decoded <- forMn at' (getAudioFile x)
    return (decoded, at_doubled, x)

getAudioFile :: Word8 -> Int -> (Offset, Word32) -> SGet B.ByteString
getAudioFile x n (o,l) =
    getSegAt o (printf "Audio file %d" n) $ do
        decypher x <$> getLazyByteString (fromIntegral l)

getXor :: Get Word8
getXor = do
    present <- getLazyByteString 4
    -- Brute force, but that's ok here
    case [ n | n <- [0..0xFF]
             , decypher n present `elem` map fst fileMagics ] of
        [] -> fail "Could not find magic hash"
        (x:_) -> return x

fileMagics :: [(B.ByteString, String)]
fileMagics =
    [ (BC.pack "RIFF", "wav")
    , (BC.pack "OggS", "ogg")
    , (BC.pack "fLaC", "flac")]

decypher :: Word8 -> B.ByteString -> B.ByteString
decypher x = B.map go
    where go 0 = 0
          go 255 = 255
          go n | n == x    = n
               | n == xor x 255 = n
               | otherwise = xor x n

getChecksum :: SGet Word32
getChecksum = do
    l <- getLength
    getSegAt (l-4) "Checksum" $ getWord32le

calcChecksum :: SGet Word32
calcChecksum = do
    l <- getLength
    bs <- getAt 0 $ getLazyByteString (fromIntegral l - 4)
    return $ B.foldl' (\s b -> fromIntegral b + s) 0 bs

array :: Integral a => Get a -> Get b -> Get [b]
array g1 g2 = do
    n <- g1
    replicateM (fromIntegral n) g2

getInitialRegs :: SGet [Word16]
getInitialRegs = do
    offset <- getSegAt 0x0018 "Initial register offset" getWord32le
    getSegAt offset "Initial registers" $
        array getWord16le getWord16le

getTipToiFile :: SGet TipToiFile
getTipToiFile = do
    id <- getSegAt 0x0014 "Product id" getWord32le
    raw_xor <- getSegAt 0x001C "Raw XOR value" getWord32le
    (comment,date) <- getSegAt 0x0020 "Comment and date" $ do
        l <- getWord8
        c <- getLazyByteString (fromIntegral l)
        d <- getLazyByteString 8
        return (c,d)
    regs <- getInitialRegs
    scripts <- getScripts
    (at, at_doubled, xor) <- getAudios
    checksum <- getChecksum
    checksumCalc <- calcChecksum
    return (TipToiFile id raw_xor comment date regs scripts at at_doubled xor checksum checksumCalc)

parseTipToiFile :: B.ByteString -> (TipToiFile, Segments)
parseTipToiFile = runSGet getTipToiFile

-- Pretty printing

lineHex bytes l = prettyHex $ runGet (extract (lineOffset l) (lineLength l)) bytes

extract :: Offset -> Word32 -> Get (B.ByteString)
extract off len = do
    skip (fromIntegral off)
    getLazyByteString (fromIntegral len)


lineOffset (Line o _ _ _) = o

lineLength :: Line -> Word32
lineLength (Line _ conds cmds audio) = fromIntegral $
    2 + 8 * length conds + 2 + 7 * length cmds + 2 + 2 * length audio

ppLine :: Transscript -> Line -> String
ppLine t (Line _ cs as xs) = spaces (map ppConditional cs) ++ ": " ++ spaces (map ppCommand as) ++ media xs
  where media [] = ""
        media _  = " " ++ ppPlayList t xs

ppPlayList :: Transscript -> PlayList -> String
ppPlayList t xs = "[" ++ commas (map (transcribe t) xs) ++ "]"

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


prettyHex :: B.ByteString -> String
prettyHex = intercalate " " . map (printf "%02X") . B.unpack

-- Utilities

forMn_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
forMn_ l f = forM_ (zip l [0..]) $ \(x,n) -> f n x

forMn :: Monad m => [a] -> (Int -> a -> m b) -> m [b]
forMn l f = forM (zip l [0..]) $ \(x,n) -> f n x

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

doubled :: Eq a => [a] -> Maybe [a]
doubled xs | take l2 xs == drop l2 xs = Just (take l2 xs)
           | otherwise                = Nothing
  where l = length xs
        l2 = l `div` 2


-- Main commands

dumpAudioTo :: FilePath -> FilePath -> IO ()
dumpAudioTo directory file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file

    printf "Audio Table entries: %d\n" (length (ttAudioFiles tt))

    createDirectoryIfMissing False directory
    forMn_ (ttAudioFiles tt) $ \n audio -> do
        let audiotype = fromMaybe "raw" $ lookup (B.take 4 audio) fileMagics
        let filename = printf "%s/%s_%04d.%s" directory (takeBaseName file) n audiotype
        if B.null audio
        then do
            printf "Skipping empty file %s...\n" filename
        else do
            B.writeFile filename audio
            printf "Dumped sample %d as %s\n" n filename

dumpScripts :: Transscript -> Bool -> Maybe Int -> FilePath -> IO ()
dumpScripts t raw sel file = do
    bytes <- B.readFile file
    let (tt,_) = parseTipToiFile bytes
        st' | Just n <- sel = filter ((== fromIntegral n) . fst) (ttScripts tt)
            | otherwise     = ttScripts tt

    forM_ st' $ \(i, ms) -> case ms of
        Nothing -> do
            printf "Script for OID %d: Disabled\n" i
        Just lines -> do
            printf "Script for OID %d:\n" i
            forM_ lines $ \line -> do
                if raw then printf "%s\n"     (lineHex bytes line)
                       else printf "    %s\n" (ppLine t line)


dumpInfo :: FilePath -> IO ()
dumpInfo file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file
    let st = ttScripts tt

    printf "Product ID: 0x%08X\n" (ttProductId tt)
    printf "Raw XOR value: 0x%08X\n" (ttRawXor tt)
    printf "Magic XOR value: 0x%02X\n" (ttAudioXor tt)
    printf "Comment: %s\n" (BC.unpack (ttComment tt))
    printf "Date: %s\n" (BC.unpack (ttDate tt))
    printf "Number of registers: %d\n" (length (ttInitialRegs tt))
    printf "Initial registers: %s\n" (show (ttInitialRegs tt))
    printf "Scripts for OIDs from %d to %d; %d/%d are disabled.\n"
        (fst (head st)) (fst (last st))
        (length (filter (isNothing . snd) st)) (length st)
    printf "Audio Table entries: %d\n" (length (ttAudioFiles tt))
    when (ttAudioFilesDoubles tt) $ printf "Audio table repeated twice\n"
    printf "Checksum found 0x%08X, calculated 0x%08X\n" (ttChecksum tt) (ttChecksumCalc tt)

lint :: FilePath -> IO ()
lint file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file

    let hyps = [ (hyp1, "play indicies are correct")
               , (hyp2 (fromIntegral (length (ttAudioFiles tt))),
                  "media indicies are correct")
               ]
    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not . hyp) (concat (mapMaybe snd (ttScripts tt)))
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> do
                printf "    %s\n" (ppLine M.empty line)

    let overlapping_segments =
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
    hyp1 (Line _ _ as mi) = all ok as
      where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
            ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                         0 <= b && b < fromIntegral (length mi)
            ok _ = True

    hyp2 :: Word16 -> Line -> Bool
    hyp2 n (Line _ _ _ mi) = all (<= n) mi



printSegment (o,l,desc) = printf "At 0x%08X Size %8d: %s\n" o l desc

segments :: FilePath -> IO ()
segments file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    mapM_ printSegment segments

findPosition :: Integer -> FilePath -> IO ()
findPosition pos' file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
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
    let (tt,segments) = parseTipToiFile bytes
    let unknown_segments =
            filter (\(o,l) -> not
                (l == 2 && runGet (skip (fromIntegral o) >> getWord16le) bytes == 0)) $
            filter (\(o,l) -> l > 0) $
            zipWith (\(o1,l1,_) (o2,_,_) -> (o1+l1, o2-(o1+l1)))
            segments (tail segments)
    printf "Unknown file segments: %d (%d bytes total)\n"
        (length unknown_segments) (sum (map snd unknown_segments))
    forM_ unknown_segments $ \(o,l) ->
        printf "   Offset: %08X to %08X (%d bytes)\n" o (o+l) l


withEachFile :: (FilePath -> IO ()) -> [FilePath] -> IO ()
withEachFile _ [] = main' undefined []
withEachFile a [f] = a f 
withEachFile a fs = forM_ fs $ \f -> do 
    printf "%s:\n" f 
    a f

type PlayState = M.Map Word8 Word16

formatState :: PlayState -> String
formatState s = spaces $
    map (\(k,v) -> printf "$%d=%d" k v) $
    filter (\(k,v) -> k == 0 || v /= 0) $
    M.toAscList s

play :: Transscript -> FilePath -> IO ()
play t file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file
    let initialState = M.fromList $ zip [0..] (ttInitialRegs tt)
    printf "Initial state (not showing zero registers): %s\n" (formatState initialState)
    forEachNumber initialState $ \i s -> do
        case lookup (fromIntegral i) (ttScripts tt) of
            Nothing -> printf "OID %d not in main table\n" i >> return s
            Just Nothing -> printf "OID %d deactivated\n" i >> return s
            Just (Just lines) -> do
                case find (enabledLine s) lines of
                    Nothing -> printf "None of these lines matched!\n" >> mapM_ (putStrLn . ppLine t) lines >> return s
                    Just l -> do
                        printf "Executing:  %s\n" (ppLine t l)
                        let s' = applyLine l s
                        printf "State now: %s\n" (formatState s')
                        return s'

enabledLine :: PlayState -> Line -> Bool
enabledLine s (Line _ cond _ _) = all (condTrue s) cond

condTrue :: PlayState -> Conditional -> Bool
condTrue s (Eq r n)  = s `value` r == n
condTrue s (NEq r n) = s `value` r /= n
condTrue s (Lt r n)  = s `value` r < n
condTrue s (GEq r n) = s `value` r >= n
condTrue _ _ = False

value :: PlayState -> Word8 -> Word16
value m r = M.findWithDefault 0 r m

applyLine :: Line -> PlayState -> PlayState
applyLine (Line _ _ act _) s = foldl' go s act
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

writeTipToi :: FilePath -> TipToiFile -> IO ()
writeTipToi out tt = do
    let bytes = runSPut (putTipToiFile tt)
    let checksum = B.foldl' (\s b -> fromIntegral b + s) 0 bytes
    B.writeFile out $ Br.toLazyByteString $
        Br.fromLazyByteString bytes `Br.append` Br.putWord32le checksum

rewrite :: FilePath -> FilePath -> IO ()
rewrite inf out = do
    (tt,_) <- parseTipToiFile <$> B.readFile inf
    writeTipToi out tt

debugGame :: IO TipToiFile
debugGame = do
    -- Files orderes so that index 0 says zero, 10 is blob
    files <- mapM B.readFile
        [ "./Audio/numbers/source/" ++ base ++ ".flac"
        | base <- [ "english-" ++ [n] | n <- ['0'..'9']] ++ ["blob" ]
        ]
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%Y%m%d" now
    return $ TipToiFile
        { ttProductId = 0x00000001 -- Bauernhof
        , ttRawXor = 0x00000039 -- dito
        , ttComment = BC.pack "created with tip-toi-reveng"
        , ttDate = BC.pack date
        , ttInitialRegs = [1]
        , ttScripts = [
            (oid, Just [line])
            | oid <- [1401..1728]
            , let chars = [oid `div` 10^p `mod` 10| p <-[3,2,1,0]]
            , let line = Line 0 [] [Play n | n <- [0..4]] ([10] ++ chars)
            ]
        , ttAudioFiles = files
        , ttAudioXor = 0xAD
        , ttAudioFilesDoubles = False
        , ttChecksum = 0x00
        , ttChecksumCalc = 0x00
        }

createDebug :: IO ()
createDebug = do
    tt <- debugGame
    writeTipToi "Debug.gme" tt


-- The main function

type Transscript = M.Map Word16 String

transcribe :: Transscript -> Word16 -> String
transcribe t idx = fromMaybe (show idx) (M.lookup idx t)

readTransscriptFile :: FilePath -> IO (M.Map Word16 String)
readTransscriptFile transcriptfile_ = do
    file <- readFile transcriptfile_
    return $ M.fromList
        [ (idx, string)
        | l <- lines file
        , (idxstr:string:_) <- return $ wordsWhen (';'==) l
        , Just idx <- return $ readMaybe idxstr
        ]

-- Avoiding dependencies, using code from http://stackoverflow.com/a/4981265/946226
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main' t ("-t":transscript:args) =
    do t2 <- readTransscriptFile transscript
       main' (t `M.union` t2) args


main' t ("info": files)             = withEachFile dumpInfo files
main' t ("media": "-d": dir: files) = withEachFile (dumpAudioTo dir) files
main' t ("media": files)            = withEachFile (dumpAudioTo "media") files
main' t ("scripts": files)          = withEachFile (dumpScripts t False Nothing) files
main' t ("script":  file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t False (Just int) file
main' t ("raw-scripts": files)      = withEachFile (dumpScripts t True Nothing) files
main' t ("raw-script": file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t True (Just int) file
main' t ("lint": files)             = withEachFile lint files
main' t ("segments": files)         = withEachFile segments files
main' t ("segment": file : n :[])
    | Just int <- readMaybe n       =              findPosition int file
    | [(int,[])] <- readHex n       =              findPosition int file
main' t ("holes": files)            = withEachFile unknown_segments files
main' t ("play": file : [])         =              play t file
main' t ("rewrite": inf : out: [])  =              rewrite inf out
main' t ("create-debug": [])        =              createDebug
main' t _ = do
    prg <- getProgName
    putStrLn $ "Usage: " ++ prg ++ " [options] command"
    putStrLn $ ""
    putStrLn $ "Options:"
    putStrLn $ "    -t <transcriptfile>"
    putStrLn $ "       replaces media file indices by a transscript"
    putStrLn $ ""
    putStrLn $ "Commands:"
    putStrLn $ "    info <file.gme>..."
    putStrLn $ "       general information"
    putStrLn $ "    media [-d dir] <file.gme>..."
    putStrLn $ "       dumps all audio samples to the given directory (default: media/)"
    putStrLn $ "    scripts <file.gme>..."
    putStrLn $ "       prints the decoded scripts for each OID"
    putStrLn $ "    script <file.gme> <n>"
    putStrLn $ "       prints the decoded scripts for the given OID"
    putStrLn $ "    raw-scripts <file.gme>..."
    putStrLn $ "       prints the scripts for each OID, in their raw form"
    putStrLn $ "    raw-script <file.gme> <n>"
    putStrLn $ "       prints the scripts for the given OID, in their raw form"
    putStrLn $ "    lint <file.gme>"
    putStrLn $ "       checks for errors in the file or in this program"
    putStrLn $ "    segments <file.gme>..."
    putStrLn $ "       lists all known parts of the file, with description."
    putStrLn $ "    segment <file.gme> <pos>"
    putStrLn $ "       which segment contains the given position."
    putStrLn $ "    holes <file.gme>..."
    putStrLn $ "       lists all unknown parts of the file."
    putStrLn $ "    play <file.gme>"
    putStrLn $ "       interactively play: Enter OIDs, and see what happens."
    putStrLn $ "    rewrite <infile.gme> <outfile.gme>"
    putStrLn $ "       parses the file and serializes it again (for debugging)."
    putStrLn $ "    create-debug"
    putStrLn $ "       creates a special Debug.gme file (compatible with Bauernhof)"
    exitFailure

main = getArgs >>= (main' M.empty)

