module Types
    ( module Types, Word8, Word16, Word32 )
where

import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Data.Map as M

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
    deriving (Eq, Show, Functor, Foldable)

data Conditional r = Cond (TVal r) CondOp (TVal r)
    deriving (Eq, Functor, Foldable)

data CondOp
    = Eq
    | Gt
    | Lt
    | GEq
    | LEq
    | NEq
    | EqAlias
      -- ^ opcode 0xFFFC. The firmware treats it exactly like Eq (0xFFF9); it has not
      --   been seen in GME files so far. Kept distinct so that re-writing a GME
      --   preserves the original bytes. Any conditional opcode not listed here is
      --   treated by the firmware as an unsatisfied (false) condition.
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
    | Rand r (TVal r)
      -- ^ opcode 0xFF00, written T($r,m) (that syntax predates knowing what the command
      --   does, and is kept for compatibility). Sets $r to (tick counter) mod (m+1) --
      --   the pen's random number source ("Rand" is the firmware's own name for it).
      --   See GME-Format.md.
    | ArmTimer Word16
      -- ^ opcode 0xFE00, written AT(m). Arms the periodic script timer: every m*100ms
      --   the pen, when idle, runs the timer script (ttTimerScript). The pen takes the
      --   period literally -- it ignores both the register field and the register/value
      --   flag of this command -- so it is a plain number here. See GME-Format.md.
    | CancelTimer
      -- ^ opcode 0xFEFF, written CT. Cancels the timer armed by ArmTimer.
    | SoundProfile Word8 (TVal r)
      -- ^ opcodes 0xFEE0..0xFEE8, written SoundProfile(p). Selects one of the pen's
      --   eight built-in sound profiles p = 0..7 (p = 8 encodes 0xFEE8, which the
      --   firmware ignores). The operand is ignored by the firmware and only kept to
      --   round-trip the bytes. Not seen in GME files so far. See GME-Format.md.
    deriving (Eq, Functor, Foldable)

type PlayList = [Word16]

data Line r = Line Offset [Conditional r] [Command r] PlayList
    deriving (Functor, Foldable)

type ProductID = Word32

data Similarity = Absent | Equal | Similar deriving (Show, Eq)

data TipToiFile = TipToiFile
    { ttProductId :: ProductID
    , ttRawXor :: Word32
    , ttComment :: B.ByteString
    , ttDate :: B.ByteString
    , ttLang :: B.ByteString
    , ttInitialRegs :: [Word16]
    , ttWelcome :: [PlayList]
    , ttScripts :: [(Word16, Maybe [Line ResReg])]
    , ttTimerScript :: [Line ResReg]
      -- ^ the timer script (the GME's "additional script table", header 0x0C). Run by
      --   the pen when the timer armed via ArmTimer (0xFE00) expires: the first line
      --   whose conditions hold is executed. Empty for most products. See GME-Format.md.
    , ttGames :: [Game]
    , ttAudioFiles :: [B.ByteString]
    , ttAudioFilesDoubles :: Similarity
    , ttAudioXor :: Word8
    , ttMediaFlags :: Maybe [Word16]
    , ttBinaries1 :: [(B.ByteString, B.ByteString)]
    , ttBinaries2 :: [(B.ByteString, B.ByteString)]
    , ttBinaries3 :: [(B.ByteString, B.ByteString)]
    , ttBinaries4 :: [(B.ByteString, B.ByteString)]
    , ttBinaries5 :: [(B.ByteString, B.ByteString)]
    , ttBinaries6 :: [(B.ByteString, B.ByteString)]
    , ttSpecialOIDs :: Maybe (Word16, Word16)
    , ttChecksum :: Word32
    , ttChecksumCalc :: Word32
    }

type PlayListList = [PlayList]
type GameId = Word16

-- Game-record fields with meanings established by firmware analysis (they hold
-- across all game types that use them):
--   * gAllNeeded  : the find-all-targets flag: 0 = the first correct tap completes
--                   the round, 1 = every target OID of the subgame must be found
--                   (word "c" of the common layout, word "i" of the type-6 layout)
--   * gRepeatOID  : a control OID that replays the current prompt or hint
--   * gEarlyRounds / gBonusEarlyRounds : round number at which the round
--                   announcement switches from the round-start to the
--                   later-round-start playlists (0 = no announcements)
-- See the game table section in GME-Format.md.
data Game =
    CommonGame
        { gGameType                 :: Word16
        , gRounds                   :: Word16
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gBonusEarlyRounds         :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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
        , gAllNeeded                :: Word16
        , gEarlyRounds              :: Word16
        , gRepeatOID                :: Word16
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

-- sgTargetOids are the correct answers, sgDecoyOids known-wrong answers with
-- dedicated feedback, sgAllOids the subgame's remaining active OIDs (hint
-- feedback). sgHeader holds ten 16-bit words tuning feedback selection and
-- wrong-tap limits. See the game table section in GME-Format.md.
data SubGame = SubGame
    { sgHeader :: B.ByteString
    , sgTargetOids :: [OID]
    , sgDecoyOids :: [OID]
    , sgAllOids :: [OID]
    , sgPlaylist :: [PlayListList]
    }
    deriving Show


type Transscript = M.Map Word16 String
type CodeMap = M.Map String Word16


-- Command options

data ImageFormat = SVG { withPNG :: Bool } | PNG | PDF
    deriving Show

suffixOf :: ImageFormat -> String
suffixOf (SVG _) = "svg"
suffixOf PNG = "png"
suffixOf PDF = "pdf"

data Conf = Conf
    { cTransscriptFile :: Maybe FilePath
    , cCodeDim :: (Int, Int)
    , cDPI :: Int
    , cPixelSize :: Int
    , cImageFormat :: Maybe ImageFormat
    }
    deriving Show

