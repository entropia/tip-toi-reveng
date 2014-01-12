{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Arrow (second)
import Debug.Trace


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

data Line = Line Offset [Conditional] [Command] [Word16]

data TipToiFile = TipToiFile
    { ttScripts :: [(Word16, Maybe [Line])]
    , ttGames :: [Game]
    , ttAudioFiles :: [B.ByteString]
    , ttAudioFilesDoubles :: Bool
    , ttAudioXor :: Word8
    , ttChecksum :: Word32
    , ttChecksumCalc :: Word32
    }


type PlayList = [Word16]
type PlayListList = [PlayList]
type GameId = Word16

data Game
    = Game6 Word16 B.ByteString [PlayListList] [SubGame] [SubGame] B.ByteString [PlayListList] PlayList
    | Game7 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList] PlayListList
    | Game8 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList] [OID] [GameId] PlayListList PlayListList
    | Game9
    | Game10
    | Game16
    | UnknownGame Word16 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList]
    deriving Show


type OID = Word16

data SubGame
    = SubGame B.ByteString [OID] [OID] [OID] [PlayListList]
    deriving Show

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
    when (offset > fromIntegral (B.length bytes)) $ do
        fail $ printf "Trying to read Segment %s from offset 0x%08X, which is after the end of the file!" desc offset
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

getPlayList :: Offset -> SGet PlayList
getPlayList offset = do
    getSegAt offset "Playlist" $ array getWord16le getWord16le

getOidList :: Offset -> SGet [OID]
getOidList offset = do
    getSegAt offset "OID list" $ array getWord16le getWord16le

getGidList :: Offset -> SGet [OID]
getGidList offset = do
    getSegAt offset "GID list" $ array getWord16le getWord16le

getPlayListList :: Offset -> SGet PlayListList
getPlayListList offset = do
    os <- getSegAt offset "Playlistlist" $ array getWord16le getWord32le
    mapM getPlayList os

getSubGame :: Offset -> SGet SubGame
getSubGame offset = do
    (u, oid1s, oid2s, oid3s, pllos) <- getSegAt offset "Subgame" $ do
        u <- getLazyByteString 20
        oid1s <- array getWord16le getWord16le
        oid2s <- array getWord16le getWord16le
        oid3s <- array getWord16le getWord16le
        pllos <- replicateM 9 getWord32le
        return (u, oid1s, oid2s, oid3s, pllos)
    plls <- mapM getPlayListList pllos
    return (SubGame u oid1s oid2s oid3s plls)

getGame :: Int -> Offset -> SGet Game
getGame n offset = do
    t <- getAt offset getWord16le
    let getHeader = getSegAt offset (printf "Game Nr. %d (Type %d)" n t)
    case t of
      6 -> do
        (u1,u2,pllos, sg1os, sg2os, u3, pll2os, plo) <- getHeader $ do
            6 <- getWord16le
            b <- getWord16le
            u1 <- getWord16le
            c <- getWord16le
            u2 <- getLazyByteString 18
            pllos <- replicateM 7 getWord32le
            sg1os <- replicateM (fromIntegral b) getWord32le
            sg2os <- replicateM (fromIntegral c) getWord32le
            u3 <- getLazyByteString 20
            pll2os <- replicateM 10 getWord32le
            plo <- getWord32le
            return (u1, u2, pllos, sg1os, sg2os, u3, pll2os, plo)
        plls <- mapM getPlayListList pllos
        sg1s <- mapM getSubGame sg1os
        sg2s <- mapM getSubGame sg2os
        pll2s <- mapM getPlayListList pll2os
        pl <- getPlayList plo
        return (Game6 u1 u2 plls sg1s sg2s u3 pll2s pl)
      7 -> do
        ((u1,c,u2,pllos, sgos, u3, pll2os), pllo) <- getHeader $ do
            c <- common
            pllo <- getWord32le
            return (c, pllo)
        plls <- mapM getPlayListList pllos
        sgs <- mapM getSubGame sgos
        pll2s <- mapM getPlayListList pll2os
        pll <- getPlayListList pllo
        return (Game7 u1 c u2 plls sgs u3 pll2s pll)
      8 -> do
        ((u1,c,u2,pllos, sgos, u3, pll2os), oidlo, gidlo, pll1o, pll2o) <- getHeader $ do
            c <- common
            oidlo <- getWord32le
            gidlo <- getWord32le
            pll1o <- getWord32le
            pll2o <- getWord32le
            return (c, oidlo, gidlo, pll1o, pll2o)
        plls <- mapM getPlayListList pllos
        sgs <- mapM getSubGame sgos
        pll2s <- mapM getPlayListList pll2os
        oidl <- getOidList oidlo
        gidl <- getGidList gidlo
        pll1 <- getPlayListList pll1o
        pll2 <- getPlayListList pll2o
        return (Game8 u1 c u2 plls sgs u3 pll2s oidl gidl pll1 pll2)

      _ -> do
        (u1,c,u2,pllos, sgos, u3, pll2os) <- getHeader common
        plls <- mapM getPlayListList pllos
        sgs <- mapM getSubGame sgos
        pll2s <- mapM getPlayListList pll2os
        return (UnknownGame t u1 c u2 plls sgs u3 pll2s)
 where
    common = do -- the common header of a non-type-6-game
        _ <- getWord16le
        b <- getWord16le
        u1 <- getWord16le
        c <- getWord16le
        u2 <- getLazyByteString 10
        pllos <- replicateM 5 getWord32le
        sgos <- replicateM (fromIntegral b) getWord32le
        u3 <- getLazyByteString 20
        pll2os <- replicateM 10 getWord32le
        return (u1, c, u2, pllos, sgos, u3, pll2os)


getGames :: SGet [Game]
getGames = do
    gameTable <- getSegAt 0x0010 "Game Table Offset" getWord32le
    gameOffsets <- getSegAt gameTable "Game Table" $
        array getWord32le getWord32le
    forMn (init gameOffsets) getGame

getTipToiFile :: SGet TipToiFile
getTipToiFile = do
    scripts <- getScripts
    games <- getGames
    (at, at_doubled, xor) <- getAudios
    checksum <- getChecksum
    checksumCalc <- calcChecksum
    return (TipToiFile scripts games at at_doubled xor checksum checksumCalc)

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

ppLine :: Line -> String
ppLine (Line _ cs as xs) = spaces (map ppConditional cs) ++ ": " ++ spaces (map ppCommand as) ++ media xs
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


ppGame :: Game -> String
ppGame (Game6 u1 u2 plls sg1s sg2s u3 pll2s pl) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists:", "%s",
                     "  subgames1:", "%s", "  subgames2:", "%s",
                     "  u3: %s",   "  playlistlists:","%s",
                     "  playlist: %s"])
    u1 (prettyHex u2) (indent 4 (map show plls))
    (concatMap ppSubGame sg1s) (concatMap ppSubGame sg2s)
    (prettyHex u3) (indent 4 (map show pll2s))
    (show pl)
ppGame (Game7 u1 c u2 plls sgs u3 pll2s pll) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists:", "%s",
                     "  subgames:", "%s",
                     "  u3: %s",   "  playlistlists:","%s",
                     "  playlistlist: %s"])
    u1 (prettyHex u2) (indent 4 (map show plls))
    (concatMap ppSubGame sgs)
    (prettyHex u3) (indent 4 (map show pll2s)) (show pll)
ppGame (Game8 u1 c u2 plls sgs u3 pll2s oidl gidl pll1 pll2) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists:", "%s",
                     "  subgames:", "%s",
                     "  u3: %s",   "  playlistlists:","%s",
                     "  oids: %s",
                     "  gids: %s",
                     "  playlistlist: %s",
                     "  playlistlist: %s"
                     ])
    u1 (prettyHex u2) (indent 4 (map show plls))
    (concatMap ppSubGame sgs)
    (prettyHex u3) (indent 4 (map show pll2s)) (show oidl) (show gidl)
    (show pll1) (show pll2)
ppGame (UnknownGame t u1 c u2 plls sgs u3 pll2s) =
    printf (unlines ["  type: %d", "  u1:   %d", "  c:    %d", "  u2:   %s",
                     "  playlistlists:", "%s", "  subgames:", "%s",
                     "  u3: %s",   "  playlistlists:","%s"])
    t u1 c (prettyHex u2) (indent 4 (map show plls))
    (concatMap ppSubGame sgs) (prettyHex u3) (indent 4 (map show pll2s))
ppGame _ = "TODO"

ppSubGame :: SubGame -> String
ppSubGame (SubGame u oids1 oids2 oids3 plls) = printf (unlines
    [ "    Subgame:"
    , "      u: %s"
    , "      oids1: %s"
    , "      oids2: %s"
    , "      oids3: %s"
    , "      playlistlists:"
    , "%s"
    ])
    (prettyHex u)
    (show oids1) (show oids2) (show oids3)
    (indent 8 (map show plls))

indent n = intercalate "\n" . map (replicate n ' ' ++)

checkLine :: Int -> Line -> [String]
checkLine n_audio l@(Line _ _ _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in line " ++ ppLine l
checkLine n_audio _ = []


prettyHex :: B.ByteString -> String
prettyHex = intercalate " " . map (printf "%02X") . B.unpack

forMn_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
forMn_ l f = forM_ (zip l [0..]) $ \(x,n) -> f n x

forMn :: Monad m => [a] -> (Int -> a -> m b) -> m [b]
forMn l f = forM (zip l [0..]) $ \(x,n) -> f n x

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

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

dumpScripts :: Bool -> Maybe Int -> FilePath -> IO ()
dumpScripts raw sel file = do
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
                       else printf "    %s\n" (ppLine line)


dumpInfo :: FilePath -> IO ()
dumpInfo file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file
    let st = ttScripts tt

    printf "Checksum found 0x%08X, calculated 0x%08X\n" (ttChecksum tt) (ttChecksumCalc tt)
    printf "Scripts for OIDs from %d to %d; %d/%d are disabled.\n"
        (fst (head st)) (fst (last st))
        (length (filter (isNothing . snd) st)) (length st)
    printf "Magic XOR value: 0x%02X\n" (ttAudioXor tt)
    when (ttAudioFilesDoubles tt) $ printf "Audio table repeated twice\n"
    printf "Audio Table entries: %d\n" (length (ttAudioFiles tt))

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
                printf "    %s\n" (ppLine line)

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


doubled :: Eq a => [a] -> Maybe [a]
doubled xs | take l2 xs == drop l2 xs = Just (take l2 xs)
           | otherwise                = Nothing
  where l = length xs
        l2 = l `div` 2



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
withEachFile _ [] = main' []
withEachFile a [f] = a f 
withEachFile a fs = forM_ fs $ \f -> do 
    printf "%s:\n" f 
    a f

type State = M.Map Word8 Word16

initialState :: State
initialState = M.singleton 0 1

formatState :: State -> String
formatState s = spaces $ map (\(k,v) -> printf "$%d=%d" k v) $ M.toAscList s

play :: FilePath -> IO ()
play file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    forEachNumber initialState $ \i s -> do
        case lookup (fromIntegral i) (ttScripts tt) of
            Nothing -> printf "OID %d not in main table\n" i >> return s
            Just Nothing -> printf "OID %d deactivated\n" i >> return s
            Just (Just lines) -> do
                case find (enabledLine s) lines of
                    Nothing -> printf "None of these lines matched!\n" >> mapM_ (putStrLn . ppLine) lines >> return s
                    Just l -> do
                        printf "Executing:  %s\n" (ppLine l)
                        let s' = applyLine l s
                        printf "State now: %s\n" (formatState s')
                        return s'

enabledLine :: State -> Line -> Bool
enabledLine s (Line _ cond _ _) = all (condTrue s) cond

condTrue :: State -> Conditional -> Bool
condTrue s (Eq r n)  = s `value` r == n
condTrue s (NEq r n) = s `value` r /= n
condTrue s (Lt r n)  = s `value` r < n
condTrue s (GEq r n) = s `value` r >= n
condTrue _ _ = False

value :: State -> Word8 -> Word16
value m r = M.findWithDefault 0 r m

applyLine :: Line -> State -> State
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

dumpGames :: FilePath -> IO ()
dumpGames file = do
    bytes <- B.readFile file
    let (tt,_) = parseTipToiFile bytes
    forMn_ (ttGames tt) $ \n g -> do
        printf "Game %d:\n" n
        printf "%s\n" (ppGame g)

main' ("info": files)             = withEachFile dumpInfo files
main' ("media": "-d": dir: files) = withEachFile (dumpAudioTo dir) files
main' ("media": files)            = withEachFile (dumpAudioTo "media") files
main' ("scripts": files)          = withEachFile (dumpScripts False Nothing) files
main' ("script":  file : n:[])
    | Just int <- readMaybe n     =             dumpScripts False (Just int) file
main' ("raw-scripts": files)      = withEachFile (dumpScripts True Nothing) files
main' ("raw-script": file : n:[])
    | Just int <- readMaybe n     =             dumpScripts True (Just int) file
main' ("games": files)            = withEachFile dumpGames files
main' ("lint": files)             = withEachFile lint files
main' ("segments": files)         = withEachFile segments files
main' ("segment": file : n :[])
    | Just int <- readMaybe n     =             findPosition int file
    | [(int,[])] <- readHex n     =             findPosition int file
main' ("holes": files)            = withEachFile unknown_segments files
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
    putStrLn $ prg ++ " raw-script <file.gme> <n>"
    putStrLn $ "       prints the scripts for the given OID, in their raw form"
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

