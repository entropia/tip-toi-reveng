{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Types
    ( module Types, Word8, Word16, Word32 )
where

import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Data.Map as M
import Data.Foldable (Foldable)

-- Main data types

type Offset = Word32
type Segment = (Offset, Word32, [String])
type Segments = [Segment]

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
    | Gt
    | Lt
    | GEq
    | LEq
    | NEq
    | Unknowncond B.ByteString
    deriving (Eq)

data ArithOp
    = Inc | Dec | Mult | Div | Mod | And | Or | XOr | Set
    deriving (Eq, Bounded, Enum)

data Command r
    = Play Word16
    | Random Word8 Word8
    | PlayAll Word8 Word8
    | PlayAllVariant (TVal r)
    | RandomVariant (TVal r)
    | Cancel
    | Game Word16
    | ArithOp ArithOp r (TVal r)
    | Neg r
    | Unknown B.ByteString r (TVal r)
    | Jump (TVal r)
    | NamedJump String -- Only in YAML files, never read from GMEs
    | Timer r (TVal r)
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
    , ttLang :: B.ByteString
    , ttInitialRegs :: [Word16]
    , ttWelcome :: [PlayList]
    , ttScripts :: [(Word16, Maybe [Line ResReg])]
    , ttGames :: [Game]
    , ttAudioFiles :: [B.ByteString]
    , ttAudioFilesDoubles :: Bool
    , ttAudioXor :: Word8
    , ttBinaries1 :: [(B.ByteString, B.ByteString)]
    , ttBinaries2 :: [(B.ByteString, B.ByteString)]
    , ttBinaries3 :: [(B.ByteString, B.ByteString)]
    , ttBinaries4 :: [(B.ByteString, B.ByteString)]
    , ttSpecialOIDs :: Maybe (Word16, Word16)
    , ttChecksum :: Word32
    , ttChecksumCalc :: Word32
    }

type PlayListList = [PlayList]
type GameId = Word16

data Game =
    CommonGame
        { gGameType                 :: Word16
        , gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        }
    | Game6
        { gRounds                   :: Word16
        , gBonusSubgameCount        :: Word16
        , gBonusRounds              :: Word16
        , gBonusTarget              :: Word16
        , gUnknownI                 :: Word16
        , gEarlyRounds              :: Word16
        , gUnknownQ                 :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gRoundStartPlayList2      :: PlayListList
        , gLaterRoundStartPlayList2 :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gBonusTargetScores        :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gBonusFinishPlayLists     :: [PlayListList]
        , gBonusSubgameIds          :: [Word16]
        }
    | Game7
        { gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gSubgameGroups            :: [[GameId]]
        }
    | Game8
        { gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gGameSelectOIDs           :: [Word16]
        , gGameSelect               :: [Word16]
        , gGameSelectErrors1        :: PlayListList
        , gGameSelectErrors2        :: PlayListList
        }
    | Game9
        { gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gExtraPlayLists           :: [PlayListList]
        }
    | Game10
        { gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gExtraPlayLists           :: [PlayListList]
        }
    | Game16
        { gRounds                   :: Word16
        , gUnknownC                 :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatLastMedia          :: Word16
        , gUnknownX                 :: Word16
        , gUnknownW                 :: Word16
        , gUnknownV                 :: Word16
        , gStartPlayList            :: PlayListList
        , gRoundEndPlayList         :: PlayListList
        , gFinishPlayList           :: PlayListList
        , gRoundStartPlayList       :: PlayListList
        , gLaterRoundStartPlayList  :: PlayListList
        , gSubgames                 :: [SubGame]
        , gTargetScores             :: [Word16]
        , gFinishPlayLists          :: [PlayListList]
        , gExtraOIDs                :: [Word16]
        , gExtraPlayLists           :: [PlayListList]
        }
    | Game253
    deriving Show


gameType :: Game -> Word16
gameType (CommonGame {gGameType = gGameType }) = gGameType
gameType Game6 {} = 6
gameType Game7 {} = 7
gameType Game8 {} = 8
gameType Game9 {} = 9
gameType Game10 {} = 10
gameType Game16 {} = 16
gameType Game253 {} = 253

type OID = Word16

data SubGame = SubGame
    { sgUnknown :: B.ByteString
    , sgOids1 :: [OID]
    , sgOids2 :: [OID]
    , sgOids3 :: [OID]
    , sgPlaylist :: [PlayListList]
    }
    deriving Show


type Transscript = M.Map Word16 String
type CodeMap = M.Map String Word16


-- Command options

data Conf = Conf
    { cTransscriptFile :: Maybe FilePath
    , cCodeDim :: (Int, Int)
    , cDPI :: Int
    , cPixelSize :: Int
    }
    deriving Show

