{-# LANGUAGE RankNTypes, RecursiveDo, RecordWildCards, GADTs, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module GMEWriter (writeTipToiFile) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as Br
import Text.Printf
import Control.Monad
import Control.Applicative (Applicative)
import qualified Data.Map as M
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Debug.Trace

import Types
import Constants
import Cypher


-- Assembling .gme files

-- Assembly monad
-- We need a data structure that we can extract its length from before we know its values
-- So we will use a lazy pair of length (Int) and builder

newtype SPutM a = SPutM (StateT Word32 (Writer Br.Builder) a)
    deriving (Functor, Applicative, Monad, MonadFix)
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
    putBS ttLang
    putWord8 0
    seek 0x0071 -- Just to be safe
    putWord32 ipllo
    seek 0x0200 -- Just to be safe
    sto <- getAddress $ putScriptTable ttScripts
    ast <- getAddress $ putWord16 0x00 -- For now, no additional script table
    gto <- getAddress $ putGameTable ttGames
    iro <- getAddress $ putInitialRegs ttInitialRegs
    mft <- getAddress $ putAudioTable ttAudioXor ttAudioFiles
    ipllo <- getAddress $ putPlayListList ttWelcome
    return ()

putGameTable :: [Game] -> SPut
putGameTable games = putOffsets putWord32 $ map putGame games

putGame :: Game -> SPut
putGame (Game6 {..}) = mdo
    putWord16 6
    putWord16 (fromIntegral (length gSubgames) - gBonusSubgameCount)
    putWord16 gRounds
    putWord16 gBonusSubgameCount
    putWord16 gBonusRounds
    putWord16 gBonusTarget
    putWord16 gUnknownI
    putWord16 gEarlyRounds
    putWord16 gUnknownQ
    putWord16 gRepeatLastMedia
    putWord16 gUnknownX
    putWord16 gUnknownW
    putWord16 gUnknownV
    putWord32 spl
    putWord32 repl
    putWord32 fpl
    putWord32 rspl
    putWord32 lrspl
    putWord32 rspl2
    putWord32 lrspl2
    mapM_ putWord32 sgo
    mapM_ putWord16 gTargetScores
    mapM_ putWord16 gBonusTargetScores
    mapM_ putWord32 fpll
    mapM_ putWord32 fpll2
    putWord32 gilo


    spl   <- getAddress $ putPlayListList gStartPlayList
    repl  <- getAddress $ putPlayListList gRoundEndPlayList
    fpl   <- getAddress $ putPlayListList gFinishPlayList
    rspl  <- getAddress $ putPlayListList gRoundStartPlayList
    lrspl <- getAddress $ putPlayListList gLaterRoundStartPlayList
    rspl2  <- getAddress $ putPlayListList gRoundStartPlayList2
    lrspl2 <- getAddress $ putPlayListList gLaterRoundStartPlayList2
    fpll  <- mapM (getAddress . putPlayListList) gFinishPlayLists
    fpll2  <- mapM (getAddress . putPlayListList) gBonusFinishPlayLists

    sgo <- mapM (getAddress . putSubGame) gSubgames

    gilo <- getAddress $ putGameIdList gBonusSubgameIds

    return ()

putGame (Game253) = mdo
    putWord16 253

putGame g = mdo
    putWord16 (gameType g)
    putWord16 (fromIntegral $ length (gSubgames g))
    putWord16 (gRounds g)
    putWord16 (gUnknownC g)
    putWord16 (gEarlyRounds g)
    putWord16 (gRepeatLastMedia g)
    putWord16 (gUnknownX g)
    putWord16 (gUnknownW g)
    putWord16 (gUnknownV g)
    putWord32 spl
    putWord32 repl
    putWord32 fpl
    putWord32 rspl
    putWord32 lrspl
    mapM_ putWord32 sgo
    mapM_ putWord16 (gTargetScores g)
    mapM_ putWord32 fpll

    case g of
        Game7 {..} -> mdo
            putWord32 sggo
            sggo <- getAddress $ do
                putOffsets putWord16 $ map putGameIdList gSubgameGroups
            return ()
        Game8 {..} -> mdo
            putWord32 gso
            putWord32 gs
            putWord32 gse1
            putWord32 gse2

            gso <- getAddress $ putOidList gGameSelectOIDs
            gs  <- getAddress $ putArray putWord16 $ map putWord16 gGameSelect
            gse1 <- getAddress $ putPlayListList gGameSelectErrors1
            gse2 <- getAddress $ putPlayListList gGameSelectErrors2

            return ()
        Game9 {..} -> mdo
            mapM_ putWord32 epll
            epll <- mapM (getAddress . putPlayListList) gExtraPlayLists
            return ()
        Game10 {..} -> mdo
            mapM_ putWord32 epll
            epll <- mapM (getAddress . putPlayListList) gExtraPlayLists
            return ()
        Game16 {..} -> mdo
            putWord32 eoids
            mapM_ putWord32 epll
            eoids <- getAddress $ putOidList gExtraOIDs
            epll <- mapM (getAddress . putPlayListList) gExtraPlayLists
            return ()
        _ -> return ()

    spl   <- getAddress $ putPlayListList $ gStartPlayList g
    repl  <- getAddress $ putPlayListList $ gRoundEndPlayList g
    fpl   <- getAddress $ putPlayListList $ gFinishPlayList g
    rspl  <- getAddress $ putPlayListList $ gRoundStartPlayList g
    lrspl <- getAddress $ putPlayListList $ gLaterRoundStartPlayList g
    fpll  <- mapM (getAddress . putPlayListList) (gFinishPlayLists g)

    sgo <- mapM (getAddress . putSubGame) (gSubgames g)
    return ()



putPlayListList :: [PlayList] -> SPut
putPlayListList playlistlist = do
    putOffsets putWord16 $ map putPlayList playlistlist

putGameIdList :: [GameId] -> SPut
putGameIdList = putArray putWord16 . map (putWord16 . (+1))

putOidList :: [GameId] -> SPut
putOidList = putArray putWord16 . map putWord16

putSubGame :: SubGame -> SPut
putSubGame (SubGame {..}) = mdo
    putBS sgUnknown
    putArray putWord16 $ map putWord16 sgOids1
    putArray putWord16 $ map putWord16 sgOids2
    putArray putWord16 $ map putWord16 sgOids3
    mapM_ putWord32 pll 
    pll  <- mapM (getAddress . putPlayListList) sgPlaylist
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
putCondOp Gt  = mapM_ putWord8 [0xFA, 0xFF]
putCondOp Lt  = mapM_ putWord8 [0xFB, 0xFF]
putCondOp GEq = mapM_ putWord8 [0xFD, 0xFF]
putCondOp LEq = mapM_ putWord8 [0xFE, 0xFF]
putCondOp NEq = mapM_ putWord8 [0xFF, 0xFF]
putCondOp (Unknowncond b) = putBS b


putCommand :: Command ResReg -> SPut
putCommand (ArithOp o r v) = do
    putWord16 r
    mapM_ putWord8 $ arithOpCode o
    putTVal v
putCommand (Neg r) = do
    putWord16 r
    mapM_ putWord8 [0xF8, 0xFF]
    putTVal (Const 0)
putCommand (RandomVariant v) = do
    putWord16 0
    mapM_ putWord8 [0xE0, 0xFF]
    putTVal v
putCommand (PlayAllVariant v) = do
    putWord16 0
    mapM_ putWord8 [0xE1, 0xFF]
    putTVal v
putCommand (Play n) = do
    putWord16 0
    mapM_ putWord8 [0xE8, 0xFF]
    putTVal (Const (fromIntegral n))
putCommand (Random a b) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFC]
    putTVal (Const (lowhigh a b))
putCommand (PlayAll a b) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFB]
    putTVal (Const (lowhigh a b))
putCommand (Game n) = do
    putWord16 0
    mapM_ putWord8 [0x00, 0xFD]
    putTVal (Const n)
putCommand Cancel = do
    putWord16 0
    mapM_ putWord8 [0xFF, 0xFA]
    putTVal (Const 0xFFFF)
putCommand (Jump v) = do
    putWord16 0
    mapM_ putWord8 [0xFF, 0xF8]
    putTVal v
putCommand (Timer r v) = do
    putWord16 r
    mapM_ putWord8 [0x00, 0xFF]
    putTVal v
putCommand (NamedJump s) = error "putCommand: Unresolved NamedJump"
putCommand (Unknown b r v) = do
    putWord16 r
    putBS b
    putTVal v

putAudioTable :: Word8 -> [B.ByteString] -> SPut
putAudioTable x as = mapFstMapSnd
    [ FunSplit (\o -> putWord32 o >> putWord32 (fromIntegral (B.length a)))
               (getAddress (putBS (cypher x a)))
    | a <- as ]

lowhigh :: Word8 -> Word8 -> Word16
lowhigh a b = fromIntegral a + fromIntegral b * 2^8

writeTipToiFile :: TipToiFile -> B.ByteString
writeTipToiFile tt = runSPut (putTipToiFile tt)
