{-# LANGUAGE GeneralizedNewtypeDeriving, RecursiveDo, ScopedTypeVariables, GADTs, RecordWildCards, DeriveGeneric, DeriveFoldable, DeriveFunctor, TypeSynonymInstances #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Binary.Builder as Br
import qualified Data.Binary.Get as G
import Data.Word
import Data.Int
import Text.Printf
import Data.Bits
import Data.List
import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Ord
import Control.Monad
import System.Directory
import Numeric (readHex)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Arrow (second)
import Data.Time
import System.Locale
import Data.Yaml hiding ((.=), Parser)
import Data.Aeson.Types hiding ((.=), Parser)
import qualified Data.Yaml as Y
import qualified Data.Text as T
import Text.Parsec hiding (Line, lookAhead, spaces)
import Text.Parsec.String
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Exception
import GHC.Generics
import Debug.Trace
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Control.Arrow

-- Main data types

data Register = RegPos Word16 | RegName String
    deriving (Show, Eq, Ord)

type ResReg = Word16

data TVal r
    = Reg r
    | Const Word16
    deriving (Eq, Functor, Foldable)

data Conditional r = Cond (TVal r) CondOp (TVal r)
    deriving (Eq, Functor, Foldable)

data CondOp
    = Eq
    | NEq
    | Lt
    | GEq
    | Unknowncond B.ByteString
    deriving (Eq)

data Command r
    = Play Word16
    | Random Word8 Word8
    | Cancel
    | Game Word16
    | Inc r (TVal r)
    | Set r (TVal r)
    | Unknown B.ByteString r (TVal r)
    deriving (Eq, Functor, Foldable)

type PlayList = [Word16]

data Line r = Line Offset [Conditional r] [Command r] PlayList
    deriving (Functor, Foldable)

type ProductID = Word32

data TipToiFile = TipToiFile
    { ttProductId :: ProductID
    , ttRawXor :: Word32
    , ttComment :: B.ByteString
    , ttDate :: B.ByteString
    , ttInitialRegs :: [Word16]
    , ttWelcome :: [PlayList]
    , ttScripts :: [(Word16, Maybe [Line ResReg])]
    , ttGames :: [Game]
    , ttAudioFiles :: [B.ByteString]
    , ttAudioFilesDoubles :: Bool
    , ttAudioXor :: Word8
    , ttChecksum :: Word32
    , ttChecksumCalc :: Word32
    }

type PlayListList = [PlayList]
type GameId = Word16

data Game
    = Game6 Word16 B.ByteString [PlayListList] [SubGame] [SubGame] B.ByteString [PlayListList] PlayList
    | Game7 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList] PlayListList
    | Game8 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList] [OID] [GameId] PlayListList PlayListList
    | Game9
    | Game10
    | Game16
    | Game253
    | UnknownGame Word16 Word16 Word16 B.ByteString [PlayListList] [SubGame] B.ByteString [PlayListList]
    deriving Show


type OID = Word16

data SubGame
    = SubGame B.ByteString [OID] [OID] [OID] [PlayListList]
    deriving Show

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
mapFstMapSnd xs = go xs (return ())
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
putTipToiFile (TipToiFile {..}) = mdo
    putWord32 sto
    putWord32 mft
    putWord32 0x238b
    putWord32 ast -- Additional script table
    putWord32 gto -- Game table offset
    putWord32 ttProductId
    putWord32 iro
    putWord32 ttRawXor
    putWord8 $ fromIntegral (B.length ttComment)
    putBS ttComment
    putBS ttDate
    seek 0x0071 -- Just to be safe
    putWord32 ipllo
    seek 0x0200 -- Just to be safe
    sto <- getAddress $ putScriptTable ttScripts
    ast <- getAddress $ putWord16 0x00 -- For now, no additional script table
    gto <- getAddress $ putGameTable
    iro <- getAddress $ putInitialRegs ttInitialRegs
    mft <- getAddress $ putAudioTable ttAudioXor ttAudioFiles
    ipllo <- getAddress $ putOffsets putWord16 $ map putPlayList ttWelcome
    return ()

putGameTable :: SPut
putGameTable = mdo
    putWord32 1 -- Hardcoded empty
    putWord32 offset
    offset <- getAddress $ do
       putWord16 253
       putWord16 0
    return ()


putScriptTable :: [(Word16, Maybe [Line ResReg])] -> SPut
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

putLines :: [Line ResReg] -> SPut
putLines = putOffsets putWord16 . map putLine

putLine :: Line ResReg -> SPut
putLine (Line _ conds acts idx) = do
    putArray putWord16 $ map putCond conds
    putArray putWord16 $ map putCommand acts
    putPlayList idx

putPlayList :: PlayList -> SPut
putPlayList = putArray putWord16 . map putWord16

putCond :: Conditional ResReg -> SPut
putCond (Cond v1 o v2) = do
    putTVal v1
    putCondOp o
    putTVal v2

putTVal :: TVal ResReg -> SPut
putTVal (Reg n) = do
    putWord8 0
    putWord16 n
putTVal (Const n) = do
    putWord8 1
    putWord16 n

putCondOp :: CondOp -> SPut
putCondOp Eq  = mapM_ putWord8 [0xF9, 0xFF]
putCondOp Lt  = mapM_ putWord8 [0xFB, 0xFF]
putCondOp GEq = mapM_ putWord8 [0xFD, 0xFF]
putCondOp NEq = mapM_ putWord8 [0xFF, 0xFF]
putCondOp (Unknowncond b) = putBS b

putCommand :: Command ResReg -> SPut
putCommand (Set r v) = do
    putWord16 r
    mapM_ putWord8 [0xF9, 0xFF]
    putTVal v
putCommand (Inc r v) = do
    putWord16 r
    mapM_ putWord8 [0xF0, 0xFF]
    putTVal v
putCommand (Play n) = do
    putWord16 0
    mapM_ putWord8 [0xE8, 0xFF]
    putTVal (Const (fromIntegral n))
putCommand (Random a b) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFC]
    putTVal (Const (lowhigh a b))
putCommand (Game n) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFD]
    putTVal (Const n)
putCommand Cancel = do
    putWord16 0
    mapM_ putWord8 [0xFF, 0xFA]
    putTVal (Const 0xFFFF)
putCommand (Unknown b r v) = do
    putWord16 r
    putBS b
    putTVal v

putAudioTable :: Word8 -> [B.ByteString] -> SPut
putAudioTable x as = mapFstMapSnd
    [ FunSplit (\o -> putWord32 o >> putWord32 (fromIntegral (B.length a)))
               (getAddress (putBS (decypher x a)))
    | a <- as ]

-- Reverse Engineering Monad

type Offset = Word32
type Segment = (Offset, Word32, [String])
type Segments = [Segment]

newtype SGet a = SGet (RWS B.ByteString [Segment] Word32  a)
    deriving (Functor, Monad)

liftGet :: G.Get a -> SGet a
liftGet act = SGet $ do
    offset <- get
    bytes <- ask
    when (offset > fromIntegral (B.length bytes)) $ do
        fail $ printf "Trying to read from offset 0x%08X, which is after the end of the file!" offset
    let (a, _, i) = G.runGetState act (B.drop (fromIntegral offset) bytes) 0
    put (offset + fromIntegral i)
    return $ a

jumpTo :: Offset -> SGet ()
jumpTo offset = SGet (put offset)

lookAhead :: SGet a -> SGet a
lookAhead (SGet act) = SGet $ do
    oldOffset <- get
    a <- act
    put oldOffset
    return a

getAt :: Offset -> (SGet a) -> SGet a
getAt offset act = lookAhead (jumpTo offset >> act)

getSeg :: String -> SGet a -> SGet a
getSeg desc (SGet act) = addStack desc $ SGet $ do
    offset <- get
    a <- censor (map addDesc) act
    newOffset <- get
    tell [(offset, newOffset - offset, [desc])]
    return a
  where addDesc (o,l,d) = (o,l,desc : d)

addStack :: String -> SGet a -> SGet a
addStack desc (SGet act1) = SGet $ rws $ \r s ->
    mapException annotate (runRWS act1 r s)
  where
    annotate (ErrorCall s) = ErrorCall (s ++ "\n when reading segment " ++ desc)

getSegAt :: Offset -> String -> SGet a -> SGet a
getSegAt offset desc act = getAt offset $ getSeg desc act

indirection :: String -> SGet a -> SGet a
indirection desc act = do
    position <- bytesRead
    offset <- getWord32
    l <- getLength
    when (offset > l) $ do
        fail $ printf "Trying to read from offset 0x%08X, mentioned at 0x%08X, which is after the end of the file!" offset position
    getSegAt offset desc act

indirectBS :: String -> SGet B.ByteString
indirectBS desc = do
    offset <- getWord32
    length <- getWord32
    getSegAt offset desc (getBS length)

maybeIndirection :: String -> SGet a -> SGet (Maybe a)
maybeIndirection desc act = do
    offset <- getWord32
    if offset == 0xFFFFFFFF
    then return Nothing
    else Just <$> getSegAt offset desc act

getLength :: SGet Word32
getLength = fromIntegral . B.length <$> getAllBytes

getAllBytes :: SGet B.ByteString
getAllBytes = SGet ask

runSGet :: SGet a -> B.ByteString -> (a, Segments)
runSGet (SGet act) bytes =
    second (sort . ((fromIntegral (B.length bytes), 0, ["End of file"]):)) $
    evalRWS act bytes 0

getWord8  = liftGet G.getWord8
getWord16 = liftGet G.getWord16le
getWord32 = liftGet G.getWord32le
getBS :: Word32 -> SGet B.ByteString
getBS n   = liftGet $ G.getLazyByteString (fromIntegral n)

bytesRead = SGet get

getArray :: Integral a => SGet a -> SGet b -> SGet [b]
getArray g1 g2 = do
    n <- g1
    replicateM (fromIntegral n) g2

getArrayN :: Integral a => SGet a -> (Int -> SGet b) -> SGet [b]
getArrayN g1 g2 = do
    n <- g1
    mapM g2 [0.. fromIntegral n - 1]

indirections :: Integral a => SGet a -> String -> SGet b -> SGet [b]
indirections g1 prefix g2 =
    getArrayN g1 (\n -> indirection (prefix ++ show n) g2)

-- Parsers

getScripts :: SGet [(Word16, Maybe [Line ResReg])]
getScripts = do
    last_code <- getWord16
    0 <- getWord16
    first_code <- getWord16
    0 <- getWord16

    forM [first_code .. last_code] $ \oid -> do
        l <- maybeIndirection (show oid) $ getScript
        return (oid,l)

getScript :: SGet [Line ResReg]
getScript = indirections getWord16 "Line " lineParser

getTVal :: SGet (TVal ResReg)
getTVal = do
    t <- getWord8
    case t of
     0 -> Reg <$> getWord16
     1 -> Const <$> getWord16
     _ -> fail $ "Unknown value tag " ++ show t

lineParser :: SGet (Line ResReg)
lineParser = begin
 where
   -- Find the occurrence of a header
    begin = do
        offset <- bytesRead

        -- Conditionals
        conds <- getArray getWord16 $ do
            v1 <- getTVal
            bytecode <- getBS 2
            let op = fromMaybe (Unknowncond bytecode) $
                     lookup bytecode conditionals
            v2 <- getTVal
            return $ Cond v1 op v2

        -- Actions
        cmds <- getArray getWord16 $ do
            r <- getWord16
            bytecode <- getBS 2
            case lookup bytecode actions of
              Just p -> p r
              Nothing -> do
                n <- getTVal
                return $ Unknown bytecode r n

        -- Audio links
        xs <- getArray getWord16 getWord16
        return $ Line offset conds cmds xs

    expectWord8 n = do
        n' <- getWord8
        when (n /= n') $ do
            b <- bytesRead
            fail $ printf "At position 0x%08X, expected %d/%02X, got %d/%02X" (b-1) n n n' n'

    conditionals =
        [ (B.pack [0xF9,0xFF], Eq  )
        , (B.pack [0xFF,0xFF], NEq )
        , (B.pack [0xFB,0xFF], Lt )
        , (B.pack [0xFD,0xFF], GEq )
        ]

    actions =
        [ (B.pack [0xE8,0xFF], \r -> do
            unless (r == 0) $ fail "Non-zero register for Play command"
            Const n <- getTVal
            return (Play n))
        , (B.pack [0x00,0xFC], \r -> do
            unless (r == 0) $ fail "Non-zero register for Random command"
            Const n <- getTVal
            return (Random (lowbyte n) (highbyte n)))
        , (B.pack [0xFF,0xFA], \r -> do
            unless (r == 0) $ fail "Non-zero register for Cancel command"
            Const 0xFFFF <- getTVal
            return Cancel)
        , (B.pack [0x00,0xFD], \r -> do
            unless (r == 0) $ fail "Non-zero register for Game command"
            Const a <- getTVal
            return (Game a))
        , (B.pack [0xF0,0xFF], \r -> do
            n <- getTVal
            return (Inc r n))
        , (B.pack [0xF9,0xFF], \r -> do
            n <- getTVal
            return (Set r n))
        ]

lowbyte, highbyte :: Word16 -> Word8
lowbyte n = fromIntegral (n `mod` 2^8)
highbyte n = fromIntegral (n `div` 2^8)

lowhigh :: Word8 -> Word8 -> Word16
lowhigh a b = fromIntegral a + fromIntegral b * 2^8

getAudios :: SGet ([B.ByteString], Bool, Word8)
getAudios = do
    until <- lookAhead getWord32
    x <- lookAhead $ jumpTo until >> getXor
    offset <- bytesRead
    let n_entries = fromIntegral ((until - offset) `div` 8)
    at_doubled <- lookAhead $ do
        half1 <- getBS (n_entries * 8 `div` 2)
        half2 <- getBS (n_entries * 8 `div` 2)
        return $ half1 == half2
    let n_entries' | at_doubled = n_entries `div` 2
                   | otherwise  = n_entries
    decoded <- forM [0..n_entries'-1] $ \n -> do
        decypher x <$> indirectBS (show n)
    -- Fix segment
    when at_doubled $ lookAhead $ getSeg "Audio table copy" $
        replicateM_ (fromIntegral n_entries') (getWord32 >> getWord32)

    return (decoded, at_doubled, x)

getXor :: SGet Word8
getXor = do
    present <- getBS 4
    -- Brute force, but that's ok here
    case [ n | n <- [0..0xFF]
             , decypher n present `elem` map fst fileMagics ] of
        [] -> fail "Could not find magic hash"
        (x:_) -> return x

fileMagics :: [(B.ByteString, String)]
fileMagics =
    [ (BC.pack "RIFF", "wav")
    , (BC.pack "OggS", "ogg")
    , (BC.pack "fLaC", "flac")
    , (BC.pack "ID3",  "mp3")]

decodeOnlyFiles :: [String]
decodeOnlyFiles = []

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
    getSegAt (l-4) "Checksum" $ getWord32

calcChecksum :: SGet Word32
calcChecksum = do
    l <- getLength
    bs <- getAt 0 $ getBS (fromIntegral l - 4)
    return $ B.foldl' (\s b -> fromIntegral b + s) 0 bs

getPlayList :: SGet PlayList
getPlayList = getArray getWord16 getWord16

getOidList :: SGet [OID]
getOidList = getArray getWord16 getWord16

getGidList :: SGet [OID]
getGidList = getArray getWord16 getWord16

getPlayListList :: SGet PlayListList
getPlayListList = indirections getWord16 "" getPlayList

getSubGame :: SGet SubGame
getSubGame = do
    u <- getBS 20
    oid1s <- getOidList
    oid2s <- getOidList
    oid3s <- getOidList
    plls <- indirections (return 9) "playlistlist " getPlayListList
    return (SubGame u oid1s oid2s oid3s plls)

getGame :: SGet Game
getGame = do
    t <- getWord16
    case t of
      6 -> do
        b <- getWord16
        u1 <- getWord16
        c <- getWord16
        u2 <- getBS 18
        plls <- indirections (return 7) "playlistlistA-" getPlayListList
        sg1s <- indirections (return b) "subgameA-" getSubGame
        sg2s <- indirections (return c) "subgameB-" getSubGame
        u3 <- getBS 20
        pll2s <- indirections (return 10) "playlistlistB-" getPlayListList
        pl <- indirection "playlist" getPlayList

        return (Game6 u1 u2 plls sg1s sg2s u3 pll2s pl)
      7 -> do
        (u1,c,u2,plls, sgs, u3, pll2s) <- common
        pll <- indirection "playlistlist" getPlayListList
        return (Game7 u1 c u2 plls sgs u3 pll2s pll)
      8 -> do
        (u1,c,u2,plls, sgs, u3, pll2s) <- common
        oidl <- indirection "oidlist" getOidList
        gidl <- indirection "gidlist" getGidList
        pll1 <- indirection "playlistlist1" getPlayListList
        pll2 <- indirection "playlistlist2" getPlayListList
        return (Game8 u1 c u2 plls sgs u3 pll2s oidl gidl pll1 pll2)

      253 -> do
        return Game253

      _ -> do
        (u1,c,u2,plls, sgs, u3, pll2s) <- common
        return (UnknownGame t u1 c u2 plls sgs u3 pll2s)
 where
    common = do -- the common header of a non-type-6-game
        b <- getWord16
        u1 <- getWord16
        c <- getWord16
        u2 <- getBS 10
        plls <- indirections (return 5) "playlistlistA-" getPlayListList
        sgs <- indirections (return b) "subgame-" getSubGame
        u3 <- getBS 20
        pll2s <- indirections (return 10) "playlistlistB-" getPlayListList
        return (u1, c, u2, plls, sgs, u3, pll2s)


getInitialRegs :: SGet [Word16]
getInitialRegs = getArray getWord16 getWord16

getTipToiFile :: SGet TipToiFile
getTipToiFile = getSegAt 0x00 "Header" $ do
    scripts <- indirection "Scripts" getScripts
    (at, at_doubled, xor) <- indirection "Media" getAudios
    _ <- getWord32 -- Usually 0x0000238b
    _ <- indirection "Additional script" getScript
    games <- indirection "Games" $ indirections getWord32 "" getGame
    id <- getWord32
    regs <- indirection "Initial registers" getInitialRegs
    raw_xor <- getWord32
    (comment,date) <- do
        l <- getWord8
        c <- getBS (fromIntegral l)
        d <- getBS 8
        return (c,d)
    checksum <- getChecksum
    checksumCalc <- calcChecksum
    ipll <- getSegAt 0x71 "After header" $ indirection "initial play lists" $ getPlayListList
    return (TipToiFile id raw_xor comment date regs ipll scripts games at at_doubled xor checksum checksumCalc)

parseTipToiFile :: B.ByteString -> (TipToiFile, Segments)
parseTipToiFile = runSGet getTipToiFile

-- Pretty printing

lineHex bytes l = prettyHex $ extract (lineOffset l) (lineLength l) bytes

extract :: Offset -> Word32 -> B.ByteString ->  B.ByteString
extract off len = B.take  (fromIntegral len) . B.drop (fromIntegral off)

lineOffset (Line o _ _ _) = o

lineLength :: Line r -> Word32
lineLength (Line _ conds cmds audio) = fromIntegral $
    2 + 8 * length conds + 2 + 7 * length cmds + 2 + 2 * length audio

ppLine :: Transscript -> Line ResReg -> String
ppLine t (Line _ cs as xs) = spaces $
    map ppConditional cs ++ map (ppCommand False t xs) as

-- Varaint that does not generate invalid play commands
exportLine :: Line ResReg -> String
exportLine (Line _ cs as xs) = spaces $
    map ppConditional cs ++ map (ppCommand True M.empty xs) as

-- Group consecutive runs of numbers, if they do not have a description
groupRuns :: (Eq a, Enum a) => (a -> Maybe b) -> [a] -> [Either b [a]]
groupRuns l = go
  where
    go []  = []
    go [x] = case l x of Nothing -> [Right [x]]
                         Just s  -> [Left s]
    go (x:xs) = case l x of
        Just l -> Left l : go xs
        Nothing -> case go xs of
            Right (y:ys):r' | succ x == y -> Right (x:y:ys) : r'
            r                             -> Right [x] : r

ppPlayList :: Transscript -> PlayList -> String
ppPlayList t xs = "[" ++ commas (map go (groupRuns (flip M.lookup t) xs)) ++ "]"
  where go (Left s) = quote s
        go (Right [])  = error "Empty list in groupRuns result"
        go (Right l)   | length l > 3 = show (head l) ++ ".." ++ show (last l)
                       | otherwise    = commas (map show l)

ppOidList :: [OID] -> String
ppOidList xs = "[" ++ commas (map go (groupRuns (const Nothing) xs)) ++ "]"
  where go (Left s) = s
        go (Right [])  = error "Empty list in groupRuns result"
        go (Right l)   | length l > 3 = show (head l) ++ ".." ++ show (last l)
                       | otherwise    = commas (map show l)

ppPlayListList :: Transscript -> PlayListList -> String
ppPlayListList t xs = "[" ++ commas (map (ppPlayList t) xs) ++ "]"

ppConditional :: Conditional ResReg -> String
ppConditional (Cond v1 o v2) = printf "%s%s%s?" (ppTVal v1) (ppCondOp o) (ppTVal v2)

ppCondOp :: CondOp -> String
ppCondOp Eq              = "=="
ppCondOp NEq             = "!="
ppCondOp Lt              = "< "
ppCondOp GEq             = ">="
ppCondOp (Unknowncond b) = printf "?%s?" (prettyHex b)

ppTVal :: Reg r => TVal r -> String
ppTVal (Reg r)   =  ppReg r
ppTVal (Const n) =  show n


ppResReg :: ResReg -> String
ppResReg n = "$" ++ show n

ppRegister :: Register -> String
ppRegister (RegPos n) = "$" ++ show n
ppRegister (RegName n) = "$" ++ n

class Reg a where
  ppReg :: a -> String

instance Reg ResReg   where ppReg = ppResReg
instance Reg Register where ppReg = ppRegister


ppCommand :: Reg r => Bool -> Transscript -> PlayList -> Command r -> String
ppCommand True t xs (Play n)     | not (validIndex xs (fromIntegral n)) = ""
ppCommand True t xs (Random a b) | any (not . validIndex xs . fromIntegral) [b..a] = ""

ppCommand _ t xs (Play n)        = printf "P(%s)" (ppPlayIndex t xs (fromIntegral n))
ppCommand _ t xs (Random a b)    = printf "P(%s)" $ commas $ map (ppPlayIndex t xs . fromIntegral ) [b..a]
ppCommand _ t xs (Cancel)        = printf "C"
ppCommand _ t xs (Game b)        = printf "G(%d)" b
ppCommand _ t xs (Inc r n)       = printf "%s+=%s" (ppReg r) (ppTVal n)
ppCommand _ t xs (Set r n)       = printf "%s:=%s" (ppReg r) (ppTVal n)
ppCommand _ t xs (Unknown b r n) = printf "?(%s,%s) (%s)" (ppReg r) (ppTVal n) (prettyHex b)

validIndex :: PlayList -> Int -> Bool
validIndex xs n = n >= 0 && n < length xs

ppPlayIndex :: Transscript -> PlayList -> Int -> String
ppPlayIndex t xs n | validIndex xs n = transcribe t (xs !! n)
                   | otherwise       = "invalid_index_" ++ show n

spaces = intercalate " "
commas = intercalate ","
quote s = printf "'%s'" s

ppGame :: Transscript -> Game -> String
ppGame t (Game6 u1 u2 plls sg1s sg2s u3 pll2s pl) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames1: (%d)", "%s",
                     "  subgames2: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  playlist: %s"])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sg1s)   (concatMap (ppSubGame t) sg1s)
    (length sg2s)   (concatMap (ppSubGame t) sg2s)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (show pl)
ppGame t (Game7 u1 c u2 plls sgs u3 pll2s pll) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  playlistlist: %s"])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (ppPlayListList t pll)
ppGame t (Game8 u1 c u2 plls sgs u3 pll2s oidl gidl pll1 pll2) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  oids: %s",
                     "  gids: %s",
                     "  playlistlist: %s",
                     "  playlistlist: %s"
                     ])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (ppOidList oidl) (show gidl)
    (ppPlayListList t pll1) (ppPlayListList t pll2)
ppGame t (UnknownGame typ u1 c u2 plls sgs u3 pll2s) =
    printf (unlines ["  type: %d",
                     "  u1:   %d",
                     "  c:    %d",
                     "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s"])
    typ u1 c (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
ppGame t Game253 =
    printf (unlines ["  type: 253"
                     ])
ppGame t _ = "TODO"

ppSubGame :: Transscript -> SubGame -> String
ppSubGame t (SubGame u oids1 oids2 oids3 plls) = printf (unlines
    [ "    Subgame:"
    , "      u: %s"
    , "      oids1: %s"
    , "      oids2: %s"
    , "      oids3: %s"
    , "      playlistlists: (%d)" , "%s"
    ])
    (prettyHex u)
    (ppOidList oids1) (ppOidList oids2) (ppOidList oids3)
    (length plls)  (indent 8 (map (ppPlayListList t) plls))

indent n = intercalate "\n" . map (replicate n ' ' ++)

checkLine :: Int -> Line ResReg -> [String]
checkLine n_audio l@(Line _ _ _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in line " ++ ppLine M.empty l
checkLine n_audio _ = []


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

-- Main commands

dumpAudioTo :: FilePath -> FilePath -> IO ()
dumpAudioTo directory file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file

    printf "Audio Table entries: %d\n" (length (ttAudioFiles tt))

    createDirectoryIfMissing False directory
    forMn_ (ttAudioFiles tt) $ \n audio -> do
        let audiotype = maybe "raw" snd $ find (\(m,t) -> m `B.isPrefixOf` audio) fileMagics
        let filename = printf "%s/%s_%d.%s" directory (takeBaseName file) n audiotype
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


dumpInfo :: Transscript -> FilePath -> IO ()
dumpInfo t file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file
    let st = ttScripts tt

    printf "Product ID: 0x%08X\n" (ttProductId tt)
    printf "Raw XOR value: 0x%08X\n" (ttRawXor tt)
    printf "Magic XOR value: 0x%02X\n" (ttAudioXor tt)
    printf "Comment: %s\n" (BC.unpack (ttComment tt))
    printf "Date: %s\n" (BC.unpack (ttDate tt))
    printf "Number of registers: %d\n" (length (ttInitialRegs tt))
    printf "Initial registers: %s\n" (show (ttInitialRegs tt))
    printf "Initial sounds: %s\n" (ppPlayListList t (ttWelcome tt))
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
            o1 l1 (ppDesc d1) o2 l2 (ppDesc d2) (o1 + l1 - o2)
  where
    hyp1 :: Line ResReg -> Bool
    hyp1 (Line _ _ as mi) = all ok as
      where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
            ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                         0 <= b && b < fromIntegral (length mi)
            ok _ = True

    hyp2 :: Word16 -> Line ResReg -> Bool
    hyp2 n (Line _ _ _ mi) = all (<= n) mi


ppDesc :: [String] -> String
ppDesc = intercalate "/"


printSegment (o,l,desc) = printf "At 0x%08X Size %8d: %s\n" o l (ppDesc desc)

segments :: FilePath -> IO ()
segments file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    mapM_ printSegment segments

explain :: FilePath -> IO ()
explain file = do
    bytes <- B.readFile file
    let (tt,segments) = parseTipToiFile bytes
    forM_ (addHoles segments) $ \e -> case e of
        Left (o,l) -> do
            printSegment (o,l,["-- unknown --"])
            printExtract bytes o l
            putStrLn ""
        Right s@(o,l,_) -> do
            printSegment s
            printExtract bytes o l
            putStrLn ""

printExtract :: B.ByteString -> Offset -> Word32 -> IO ()
printExtract b o l = do
    let o1 = o .&. 0xFFFFFFF0
    lim_forM_ [o1, o1+0x10 .. (o + l-1)] $ \s -> do
        let s' = max o s
        let d  = fromIntegral s' - fromIntegral s
        let l' = (min (o + l) (s + 0x10)) - s'
        printf "   0x%08X: %s%s\n"
            s
            (replicate (d*3) ' ')
            (prettyHex (extract s' l' b))
  where
    lim_forM_ l act
        = if length l > 30
          then do act (head l)
                  printf "   (skipping %d lines)\n" (length l - 2) :: IO ()
                  act (last l)
          else do forM_ l act

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


addHoles :: [Segment] -> [Either (Offset, Word32) Segment]
addHoles = go
   where go [] = []
         go [s] = [Right s]
         go (s@(o1,l1,d2):r@((o2,_,_):_))
            | o1 + l1 == o2 -- no hole
            = Right s : go r
            | otherwise -- a hole
            = Right s : Left (o1+l1, o2 - (o1 + l1)) : go r

unknown_segments :: FilePath -> IO ()
unknown_segments file = do
    bytes <- B.readFile file
    let (_,segments) = parseTipToiFile bytes
    let unknown_segments =
            filter (\(o,l) -> not
                (l == 2 && G.runGet (G.skip (fromIntegral o) >> G.getWord16le) bytes == 0)) $
            lefts $ addHoles $ segments
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

type PlayState r = M.Map r Word16

formatState :: PlayState ResReg -> String
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

enabledLine :: Ord r => PlayState r -> Line r -> Bool
enabledLine s (Line _ cond _ _) = all (condTrue s) cond

condTrue :: Ord r => PlayState r -> Conditional r -> Bool
condTrue s (Cond v1 o v2) = value s v1 =?= value s v2
  where
    (=?=) = case o of
        Eq  -> (==)
        NEq -> (/=)
        Lt  -> (<)
        GEq -> (>=)
        _   -> \_ _ -> False

value :: Ord r => PlayState r -> TVal r -> Word16
value m (Reg r) = M.findWithDefault 0 r m
value m (Const n) = n

applyLine :: Ord r => Line r -> PlayState r -> PlayState r
applyLine (Line _ _ act _) s = foldl' go s act
  where go s (Set r n) = M.insert r (s `value` n) s
        go s (Inc r n) = M.insert r (s `value` (Reg r) + s `value` n) s
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

dumpGames :: Transscript -> FilePath -> IO ()
dumpGames t file = do
    bytes <- B.readFile file
    let (tt,_) = parseTipToiFile bytes
    forMn_ (ttGames tt) $ \n g -> do
        printf "Game %d:\n" n
        printf "%s\n" (ppGame t g)

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

export :: FilePath -> FilePath -> IO ()
export inf out = do
    (tt,_) <- parseTipToiFile <$> B.readFile inf
    let tty = tt2ttYaml (printf "media/%s_%%s" (takeBaseName inf)) tt
    encodeFile out tty


data TipToiYAML = TipToiYAML
    { ttyScripts :: M.Map String [String]
    , ttyComment :: Maybe String
    , ttyMedia_Path :: Maybe String
    , ttyInit :: Maybe String
    , ttyWelcome :: Maybe String
    , ttyProduct_Id :: Word32
    }
    deriving Generic

options = defaultOptions { fieldLabelModifier = map fix . map toLower . drop 3 }
       where fix '_' = '-'
             fix c   = c

instance FromJSON TipToiYAML where
     parseJSON = genericParseJSON $ options
instance ToJSON TipToiYAML where
     toJSON = genericToJSON $ options

lexer       = P.makeTokenParser emptyDef

parseLine :: Parser ([Word16] -> Line Register, [String])
parseLine = do
    conds <- many (P.try parseCond)
    (acts, filenames) <- parseCommands 0
    eof
    return (Line 0 conds acts, filenames)

descP d p = p <?> d

parseCond :: Parser (Conditional Register)
parseCond = descP "Conditional" $ do
    v1 <- parseTVal
    op <- parseCondOp
    v2 <- parseTVal
    P.lexeme lexer (char '?')
    return (Cond v1 op v2)

parseWord16 :: Parser Word16
parseWord16 = fromIntegral <$> P.natural lexer

parseReg :: Parser Register
parseReg = char '$' >> (RegPos <$> parseWord16 <|> RegName <$> many1 (alphaNum <|> char '_'))

parseTVal :: Parser (TVal Register)
parseTVal = (Reg <$> parseReg <|> Const <$> parseWord16) <?> "Value"

parseCondOp :: Parser CondOp
parseCondOp = choice
    [ P.reservedOp lexer "==" >> return Eq
    , P.reservedOp lexer "/=" >> return NEq
    , P.reservedOp lexer "!=" >> return NEq
    , P.reservedOp lexer "<"  >> return Lt
    , P.reservedOp lexer ">=" >> return GEq
    ]

parseInitRegs :: Parser [(Register, Word16)]
parseInitRegs = many $ do
    r <- parseReg
    P.reservedOp lexer ":="
    v <- parseWord16
    return (r,v)

parseWelcome :: Parser [String]
parseWelcome = P.commaSep lexer $ parseAudioRef

parseAudioRef :: Parser String
parseAudioRef = P.lexeme lexer $ many1 (alphaNum <|> char '_')

parseCommands :: Int -> Parser ([Command Register], [String])
parseCommands i =
    choice
    [ eof >> return ([],[])
    , descP "Register action" $
      do r <- parseReg
         op <- choice [ P.reservedOp lexer ":=" >> return Set
                      , P.reservedOp lexer "+=" >> return Inc ]
         v <- parseTVal
         (cmds, filenames) <- parseCommands i
         return (op r v : cmds, filenames)
    , descP "Play action" $
      do char 'P'
         fns <- P.parens lexer $ P.commaSep1 lexer parseAudioRef
         let n = length fns
         (cmds, filenames) <- parseCommands (i+n)
         let c = case fns of
                [fn] -> Play (fromIntegral i)
                _    -> Random (fromIntegral (i + n - 1)) (fromIntegral i)
         return (c : cmds, fns ++ filenames)
    , descP "Cancel" $
      do char 'C'
         (cmds, filenames) <- parseCommands i
         return (Cancel : cmds, filenames)
    , descP "Start Game" $
      do char 'G'
         n <- P.parens lexer $ parseWord16
         (cmds, filenames) <- parseCommands i
         return (Game n : cmds, filenames)
    ]

resolveRegs :: (M.Map Register Word16, [(t0, Maybe [Line Register])]) -> (M.Map ResReg Word16, [(t0, Maybe [Line ResReg])])
resolveRegs x = everywhere x
  where
    -- Could use generics somehow
    regs = S.fromList $
        M.keys (fst x) ++ concatMap (maybe [] (concatMap F.toList) . snd) (snd x)
    -- Could use generics somehow
    everywhere = M.mapKeys resolve *** map (second (fmap (map (fmap resolve))))


    regNums = S.fromList [ n | RegPos n <- S.toList regs ]
    regNames = [ n | RegName n <- S.toList regs ]
    mapping = M.fromList $ zip regNames [n | n <- [0..], n `S.notMember` regNums]
    resolve (RegPos n) = n
    resolve (RegName n) = fromMaybe (error "resolveRegs broken") (M.lookup n mapping)



tt2ttYaml :: String -> TipToiFile -> TipToiYAML
tt2ttYaml path (TipToiFile {..}) = TipToiYAML
    { ttyProduct_Id = ttProductId
    , ttyInit = Just $ spaces $ [ ppCommand True M.empty [] (Set (RegPos r) (Const n))
                                | (r,n) <- zip [0..] ttInitialRegs , n /= 0]
    , ttyWelcome = Just $ commas $ map show $ concat ttWelcome
    , ttyComment = Just $ BC.unpack ttComment
    , ttyScripts = M.fromList
        [ (show oid, map exportLine ls) | (oid, Just ls) <- ttScripts]
    , ttyMedia_Path = Just path
    }

ttYaml2tt :: FilePath -> TipToiYAML -> IO TipToiFile
ttYaml2tt dir (TipToiYAML {..}) = do
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%Y%m%d" now

    let m = M.mapKeys read ttyScripts
        first = fst (M.findMin m)
        last = fst (M.findMax m)

    welcome_names <- case P.parse parseWelcome "welcome" (fromMaybe "" ttyWelcome) of
        Left e ->  fail (show e)
        Right l -> return l

    (prescripts, filenames) <- liftM unzip $ forM [first .. last] $ \oid -> do
       case M.lookup oid m of
        Nothing -> return (\_ -> (oid, Nothing), [])
        Just raw_lines -> do
            (lines, filenames) <- liftM unzip $ forMn raw_lines $ \i raw_line ->
                let d = printf "Line %d of OID %d" i oid
                in case P.parse parseLine d raw_line of
                    Left e ->       fail (show e)
                    Right (l, s) -> return (\f -> l (map f s), s)
            return (\f -> (oid, Just (map ($ f) lines)), concat filenames)

    let filenames' = S.toList $ S.fromList $ welcome_names ++ concat filenames
    let filename_lookup = (M.fromList (zip filenames' [0..]) M.!)

    let welcome = [map filename_lookup welcome_names]

    preInitRegs <- case P.parse parseInitRegs "init" (fromMaybe "" ttyInit) of
        Left e ->  fail (show e)
        Right l -> return $ M.fromList l

    -- resolve registers
    let (initRegs, scripts) = resolveRegs (preInitRegs, map ($ filename_lookup) prescripts)

    let maxReg = maximum
            [ r
            | (_, Just ls) <- scripts
            , Line _ cs as _ <- ls
            , r <- concatMap F.toList cs ++ concatMap F.toList as ]



    files <- forM filenames' $ \fn -> do
        let paths = [ combine dir relpath
                    | ext <- map snd fileMagics ++ decodeOnlyFiles
                    , let pat = fromMaybe "%s" ttyMedia_Path
                    , let relpath = printf pat fn <.> ext
                    ]
        ex <- filterM doesFileExist paths
        case ex of
            [] -> do
                putStrLn "Could not find any of these files:"
                mapM_ putStrLn paths
                exitFailure
            [f] -> B.readFile f
            _  -> do
                putStrLn "Multiple matching files found:"
                mapM_ putStrLn ex
                exitFailure

    return $ TipToiFile
        { ttProductId = ttyProduct_Id
        , ttRawXor = 0x00000039 -- from Bauernhof
        , ttComment = BC.pack (fromMaybe "created with tip-toi-reveng" ttyComment)
        , ttDate = BC.pack date
        , ttWelcome = welcome
        , ttInitialRegs = [fromMaybe 0 (M.lookup r initRegs) | r <- [0..maxReg]]
        , ttScripts = scripts
        , ttGames = []
        , ttAudioFiles = files
        , ttAudioXor = 0xAD
        , ttAudioFilesDoubles = False
        , ttChecksum = 0x00
        , ttChecksumCalc = 0x00
        }

assemble :: FilePath -> FilePath -> IO ()
assemble inf out = do
    etty <- decodeFileEither inf
    case etty of
        Left e -> print e
        Right tty -> do
            tt <- ttYaml2tt (takeDirectory inf) tty
            writeTipToi out tt


debugGame :: ProductID -> IO TipToiFile
debugGame productID = do
    -- Files orderes so that index 0 says zero, 10 is blob
    files <- mapM B.readFile
        [ "./Audio/digits/" ++ base ++ ".ogg"
        | base <- [ "english-" ++ [n] | n <- ['0'..'9']] ++ ["blob" ]
        ]
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%Y%m%d" now
    return $ TipToiFile
        { ttProductId = productID
        , ttRawXor = 0x00000039 -- from Bauernhof
        , ttComment = BC.pack "created with tip-toi-reveng"
        , ttDate = BC.pack date
        , ttWelcome = []
        , ttInitialRegs = [1]
        , ttScripts = [
            (oid, Just [line])
            | oid <- [1..15000]
            , let chars = [oid `div` 10^p `mod` 10| p <-[4,3,2,1,0]]
            , let line = Line 0 [] [Play n | n <- [0..5]] ([10] ++ chars)
            ]
        , ttGames = []
        , ttAudioFiles = files
        , ttAudioXor = 0xAD
        , ttAudioFilesDoubles = False
        , ttChecksum = 0x00
        , ttChecksumCalc = 0x00
        }

createDebug :: FilePath -> ProductID -> IO ()
createDebug out productID = do
    tt <- debugGame productID
    writeTipToi out tt


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

main' t ("export": inf : [] )       = main' t ("export":inf: dropExtension inf <.> "yaml":[])
main' t ("assemble": inf : [] )     = main' t ("assemble":inf: dropExtension inf <.> "gme":[])

main' t ("info": files)             = withEachFile (dumpInfo t) files
main' t ("media": "-d": dir: files) = withEachFile (dumpAudioTo dir) files
main' t ("media": files)            = withEachFile (dumpAudioTo "media") files
main' t ("scripts": files)          = withEachFile (dumpScripts t False Nothing) files
main' t ("script":  file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t False (Just int) file
main' t ("raw-scripts": files)      = withEachFile (dumpScripts t True Nothing) files
main' t ("raw-script": file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t True (Just int) file
main' t ("games": files)            = withEachFile (dumpGames t) files
main' t ("lint": files)             = withEachFile lint files
main' t ("segments": files)         = withEachFile segments files
main' t ("segment": file : n :[])
    | Just int <- readMaybe n       =              findPosition int file
    | [(int,[])] <- readHex n       =              findPosition int file
main' t ("holes": files)            = withEachFile unknown_segments files
main' t ("explain": files)          = withEachFile explain files
main' t ("play": file : [])         =              play t file
main' t ("rewrite": inf : out: [])  =              rewrite inf out
main' t ("export": inf : out: [] )  =              export inf out
main' t ("assemble": inf : out: [] )  =              assemble inf out
main' t ("create-debug": out : n :[])
    | Just int <- readMaybe n       =              createDebug out int
    | [(int,[])] <- readHex n       =              createDebug out int
main' _ _ = do
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
    putStrLn $ "    games <file.gme>..."
    putStrLn $ "       prints the decoded games"
    putStrLn $ "    lint <file.gme>"
    putStrLn $ "       checks for errors in the file or in this program"
    putStrLn $ "    segments <file.gme>..."
    putStrLn $ "       lists all known parts of the file, with description."
    putStrLn $ "    segment <file.gme> <pos>"
    putStrLn $ "       which segment contains the given position."
    putStrLn $ "    holes <file.gme>..."
    putStrLn $ "       lists all unknown parts of the file."
    putStrLn $ "    explain <file.gme>..."
    putStrLn $ "       lists all parts of the file, with description and hexdum and hexdump."
    putStrLn $ "    play <file.gme>"
    putStrLn $ "       interactively play: Enter OIDs, and see what happens."
    putStrLn $ "    rewrite <infile.gme> <outfile.gme>"
    putStrLn $ "       parses the file and serializes it again (for debugging)."
    putStrLn $ "    create-debug <outfile.gme> <productid>"
    putStrLn $ "       creates a special Debug.gme file for that productid"
    putStrLn $ "    export <infile.gme> [<outfile.yaml>]"
    putStrLn $ "       dumps the file in the human-readable yaml format"
    putStrLn $ "    assemble <infile.yaml> <outfile.gme>"
    putStrLn $ "       creates a gme file from the given source"
    exitFailure

main = getArgs >>= (main' M.empty)

