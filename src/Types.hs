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
    | Cancel
    | Game Word16
    | ArithOp ArithOp r (TVal r)
    | Neg r
    | Unknown B.ByteString r (TVal r)
    | Jump (TVal r)
    | NamedJump String -- Only in YAML files, never read from GMEs
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
    , ttBinaries1 :: [(B.ByteString, B.ByteString)]
    , ttBinaries2 :: [(B.ByteString, B.ByteString)]
    , ttBinaries3 :: [(B.ByteString, B.ByteString)]
    , ttBinaries4 :: [(B.ByteString, B.ByteString)]
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

type Transscript = M.Map Word16 String

