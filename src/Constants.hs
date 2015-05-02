module Constants where

import qualified Data.ByteString.Lazy.Char8 as BC

import Types

arithOpCode :: ArithOp -> [Word8]
arithOpCode Inc  = [0xF0, 0xFF]
arithOpCode Dec  = [0xF1, 0xFF]
arithOpCode Mult = [0xF2, 0xFF]
arithOpCode Div  = [0xF3, 0xFF]
arithOpCode Mod  = [0xF4, 0xFF]
arithOpCode And  = [0xF5, 0xFF]
arithOpCode Or   = [0xF6, 0xFF]
arithOpCode XOr  = [0xF7, 0xFF]
-- Neg is unary, hence a Command
arithOpCode Set  = [0xF9, 0xFF]

knownRawXOR :: Word32
knownRawXOR = 0x00000039 -- from Bauernhof

knownXOR :: Word8
knownXOR = 0xAD

fileMagics :: [(BC.ByteString, String)]
fileMagics =
    [ (BC.pack "RIFF", "wav")
    , (BC.pack "OggS", "ogg")
    , (BC.pack "fLaC", "flac")
    , (BC.pack "ID3",  "mp3")]

maxCommentLength :: Int
maxCommentLength = 49
