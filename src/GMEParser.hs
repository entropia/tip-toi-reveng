{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module GMEParser (parseTipToiFile) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Binary.Get as G
import Text.Printf
import Data.List
import Data.Functor
import Control.Applicative (Applicative)
import Data.Maybe
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Exception
import Control.Arrow

import Types
import Constants
import Cypher

-- Reverse Engineering Monad

newtype SGet a = SGet (RWS B.ByteString [Segment] Word32  a)
    deriving (Functor, Applicative, Monad)

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
    if offset == 0xFFFFFFFF || offset == 0x00000000
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
        [ (B.pack [0xF9,0xFF], Eq)
        , (B.pack [0xFA,0xFF], Gt)
        , (B.pack [0xFB,0xFF], Lt)
        , (B.pack [0xFD,0xFF], GEq)
        , (B.pack [0xFE,0xFF], LEq)
        , (B.pack [0xFF,0xFF], NEq)
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
        , (B.pack [0xFF,0xF8], \r -> do
            unless (r == 0) $ fail "Non-zero register for Jump command"
            v <- getTVal
            return (Jump v))
        , (B.pack [0x00,0xFD], \r -> do
            unless (r == 0) $ fail "Non-zero register for Game command"
            Const a <- getTVal
            return (Game a))
        , (B.pack [0xF8,0xFF], \r -> do
            _ <- getTVal
            return (Neg r))
        ] ++ 
        [ (B.pack (arithOpCode o), \r -> do
            n <- getTVal
            return (ArithOp o r n))
        | o <- [minBound..maxBound]
        ]

lowbyte, highbyte :: Word16 -> Word8
lowbyte n = fromIntegral (n `mod` 2^8)
highbyte n = fromIntegral (n `div` 2^8)

getBinaries :: SGet [(B.ByteString, B.ByteString)]
getBinaries = do
    n <- getWord16
    _ <- getBS 14 -- padding
    forM [0..n - 1] $ \n -> do
        offset <- getWord32
        length <- getWord32
        desc <- getBS 8
        binary <- getSegAt offset (BC.unpack desc) (getBS length)
        return (desc, binary)

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
        cypher x <$> indirectBS (show n)
    -- Fix segment
    when at_doubled $ lookAhead $ getSeg "Audio table copy" $
        replicateM_ (fromIntegral n_entries') (getWord32 >> getWord32)

    return (decoded, at_doubled, x)

getXor :: SGet Word8
getXor = do
    present <- getBS 4
    -- Brute force, but that's ok here
    case [ n | n <- [0..0xFF]
             , cypher n present `elem` map fst fileMagics ] of
        [] -> fail "Could not find magic hash"
        (x:_) -> return x

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
    ttScripts <- indirection "Scripts" getScripts
    (ttAudioFiles, ttAudioFilesDoubles, ttAudioXor) <- indirection "Media" getAudios
    _ <- getWord32 -- Usually 0x0000238b
    _ <- indirection "Additional script" getScript
    ttGames <- indirection "Games" $ indirections getWord32 "" getGame
    ttProductId <- getWord32
    ttInitialRegs <- indirection "Initial registers" getInitialRegs
    ttRawXor <- getWord32
    (ttComment, ttDate) <- do
        l <- getWord8
        c <- getBS (fromIntegral l)
        d <- getBS 8
        return (c,d)

    jumpTo 0x0071
    ttWelcome <- indirection "initial play lists" $ getPlayListList

    jumpTo 0x0090
    ttBinaries1 <- fromMaybe [] <$> maybeIndirection "Binaries 1" getBinaries

    jumpTo 0x0098
    ttBinaries2 <- fromMaybe [] <$> maybeIndirection "Binaries 1" getBinaries

    jumpTo 0x00A0
    ttBinaries3 <- fromMaybe [] <$> maybeIndirection "Single binary 1" getBinaries

    jumpTo 0x00A8
    ttBinaries4 <- fromMaybe [] <$> maybeIndirection "Single binary 2" getBinaries

    ttChecksum <- getChecksum
    ttChecksumCalc <- calcChecksum
    return $ TipToiFile {..}

parseTipToiFile :: B.ByteString -> (TipToiFile, Segments)
parseTipToiFile = runSGet getTipToiFile


