{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module GMEParser (parseTipToiFile) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Binary.Get as G
import Text.Printf
import Data.List
import Data.Functor
import Control.Applicative (Applicative, (<*>))
import Data.Maybe
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Exception
import Control.Arrow
import Debug.Trace

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

getAt :: Offset -> SGet a -> SGet a
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

getBSNul :: SGet B.ByteString
getBSNul = liftGet G.getLazyByteStringNul

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
        [ (B.pack [0xE0,0xFF], \r -> do
            unless (r == 0) $ fail "Non-zero register for RandomVariant command"
            v <- getTVal
            return (RandomVariant v))
        , (B.pack [0xE1,0xFF], \r -> do
            unless (r == 0) $ fail "Non-zero register for PlayAllVariant command"
            v <- getTVal
            return (PlayAllVariant v))
        , (B.pack [0xE8,0xFF], \r -> do
            unless (r == 0) $ fail "Non-zero register for Play command"
            Const n <- getTVal
            return (Play n))
        , (B.pack [0x00,0xFC], \r -> do
            unless (r == 0) $ fail "Non-zero register for Random command"
            Const n <- getTVal
            return (Random (lowbyte n) (highbyte n)))
        , (B.pack [0x00,0xFB], \r -> do
            unless (r == 0) $ fail "Non-zero register for PlayAll command"
            Const n <- getTVal
            return (PlayAll (lowbyte n) (highbyte n)))
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
        , (B.pack [0x00,0xFF], \r -> do
            v <- getTVal
            return (Timer r v))
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

getAudios :: Word32 -> SGet ([B.ByteString], Bool, Word8)
getAudios rawXor = do
    until <- lookAhead getWord32
    x <- case () of
          () | rawXor == knownRawXOR -> return knownXOR
             | otherwise             -> lookAhead $ jumpTo until >> getXor
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
             , let c = cypher n present
             , (magic,_) <- fileMagics
             , magic `B.isPrefixOf` c
             ] of
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

getGameIdList :: SGet [GameId]
getGameIdList = getArray getWord16 (subtract 1 <$> getWord16)

getGameIdListList :: SGet [[GameId]]
getGameIdListList = indirections getWord16 "" getGameIdList

getOidList :: SGet [OID]
getOidList = getArray getWord16 getWord16

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
    gGameType <- getWord16
    getRealGame gGameType

getRealGame :: Word16 -> SGet Game
getRealGame 253 = return Game253
getRealGame gGameType = do
    gSubgameCount             <- getWord16
    gRounds                   <- getWord16
    gUnknownC                 <- getIf (/=6) getWord16
    gBonusSubgameCount        <- getIf (==6) getWord16
    gBonusRounds              <- getIf (==6) getWord16
    gBonusTarget              <- getIf (==6) getWord16
    gUnknownI                 <- getIf (==6) getWord16
    gEarlyRounds              <- getWord16
    gUnknownQ                 <- getIf (==6) getWord16
    gRepeatLastMedia          <- getWord16
    gUnknownX                 <- getWord16
    gUnknownW                 <- getWord16
    gUnknownV                 <- getWord16
    gStartPlayList            <- indirection "startplaylist" getPlayListList
    gRoundEndPlayList         <- indirection "roundendplaylist" getPlayListList
    gFinishPlayList           <- indirection "finishplaylist" getPlayListList
    gRoundStartPlayList       <- indirection "roundstartplaylist" getPlayListList
    gLaterRoundStartPlayList  <- indirection "laterroundstartplaylist" getPlayListList
    gRoundStartPlayList2      <- getIf (==6) $ indirection "roundendplaylist2" getPlayListList
    gLaterRoundStartPlayList2 <- getIf (==6) $ indirection "laterroundstartplaylist2" getPlayListList
    let subgameCount | gGameType == 6 = gSubgameCount + gBonusSubgameCount
                     | otherwise      = gSubgameCount
    gSubgames                 <- indirections (return subgameCount) "subgame-" getSubGame
    gTargetScores             <- if gGameType == 6 then replicateM 2 getWord16
                                                   else replicateM 10 getWord16
    gBonusTargetScores        <- getIf (==6) $ replicateM 8 getWord16
    let fplCount | gGameType == 6 = 2
                 | otherwise      = 10
    gFinishPlayLists          <- indirections (return fplCount) "finishplaylist-" getPlayListList
    gBonusFinishPlayLists     <- getIf (==6) $ indirections (return 8) "bonus finishplaylist-" getPlayListList
    gBonusSubgameIds          <- getIf (==6) $ indirection "subgameidlist" getGameIdList
    gSubgameGroups            <- getIf (==7) $ indirection "subgamegroups" getGameIdListList
    gGameSelectOIDs           <- getIf (==8) $ indirection "gameSelectOids" getOidList
    gGameSelect               <- getIf (==8) $ indirection "gameSelect" getGameIdList
    gGameSelectErrors1        <- getIf (==8) $ indirection "gameSelectErrors1" getPlayListList
    gGameSelectErrors2        <- getIf (==8) $ indirection "gameSelectErrors2" getPlayListList
    gExtraOIDs                <- getIf (==16) $ indirection "extra oids" getOidList
    gExtraPlayLists           <- case gGameType of
        9 ->  indirections (return 75) "playlist-" getPlayListList
        10 -> indirections (return 1) "playlist-" getPlayListList
        16 -> indirections (return 3) "playlist-" getPlayListList
        _ -> return $ error $ "error in gExtraPlayLists"

    case gGameType of
      6 -> return $ Game6 {..}
      7 -> return $ Game7 {..}
      8 -> return $ Game8 {..}
      9 -> return $ Game9 {..}
      10 -> return $ Game10 {..}
      16 -> return $ Game16 {..}
      253 -> return Game253
      _ -> return $ CommonGame {..}
  where
    getIf :: (Word16 -> Bool) -> SGet a -> SGet a
    getIf p a | p gGameType = a
              | otherwise   = return (error "getIf used wrongly")


getInitialRegs :: SGet [Word16]
getInitialRegs = getArray getWord16 getWord16

getSpecials :: SGet (Word16, Word16)
getSpecials = (,) <$> getWord16 <*> getWord16

getTipToiFile :: SGet TipToiFile
getTipToiFile = getSegAt 0x00 "Header" $ do
    ttScripts <- indirection "Scripts" getScripts
    ttRawXor <- getAt 0x001C getWord32
    (ttAudioFiles, ttAudioFilesDoubles, ttAudioXor) <- indirection "Media" (getAudios ttRawXor)
    _ <- getWord32 -- Usually 0x0000238b
    _ <- indirection "Additional script" getScript
    ttGames <- indirection "Games" $ indirections getWord32 "" getGame
    ttProductId <- getWord32
    ttInitialRegs <- indirection "Initial registers" getInitialRegs
    _ <- getWord32 -- raw Xor
    commentLength <- getWord8
    ttComment <- getBS (fromIntegral commentLength)
    ttDate <- getBS 8
    ttLang <- getBSNul

    jumpTo 0x0071
    ttWelcome <- indirection "initial play lists" $ getPlayListList

    jumpTo 0x0090
    ttBinaries1 <- fromMaybe [] <$> maybeIndirection "Binaries 1" getBinaries
    ttSpecialOIDs <- maybeIndirection "special symbols" getSpecials
    ttBinaries2 <- fromMaybe [] <$> maybeIndirection "Binaries 1" getBinaries

    jumpTo 0x00A0
    ttBinaries3 <- fromMaybe [] <$> maybeIndirection "Single binary 1" getBinaries
    getWord32 --ignored
    ttBinaries4 <- fromMaybe [] <$> maybeIndirection "Single binary 2" getBinaries

    ttChecksum <- getChecksum
    ttChecksumCalc <- calcChecksum
    return $ TipToiFile {..}

parseTipToiFile :: B.ByteString -> (TipToiFile, Segments)
parseTipToiFile = runSGet getTipToiFile


