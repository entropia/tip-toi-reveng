module Constants where

import qualified Data.Map as M
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


fileMagics :: [(BC.ByteString, String)]
fileMagics =
    [ (BC.pack "RIFF", "wav")
    , (BC.pack "OggS", "ogg")
    , (BC.pack "fLaC", "flac")
    , (BC.pack "ID3",  "mp3")]


knownCodes :: [Word16]
knownCodes = [4716, 4717, 4718, 4719, 4720, 4721, 4722, 4723, 4724, 4725, 4726, 4727, 4728, 4729, 4730, 4731, 4732, 4733, 4734, 4735, 4736, 4737, 4738, 4739, 4740, 4741, 4742, 4743, 4744, 4745, 4746, 4747, 4748, 4749, 4750, 4751, 4752, 4753, 4754, 4755, 4756, 4757, 4758, 4759, 4760, 4761, 4762, 4763, 4764, 4765, 4766, 4767, 4768, 4769, 4770, 4771, 4772, 4773, 4774, 4775, 4776, 4777, 4778, 4779, 4780, 4781, 4782, 4783, 4784, 4785, 4786, 4787, 4788, 4789, 4790, 4791, 4792, 4793, 4794, 4795, 4796, 4797, 4798, 4799, 4800, 4801, 4802, 4803, 4804, 4805, 4806, 4807, 4808, 4809, 4810, 4811, 4812, 4813, 4814, 4815]
--knownCodes = [8066..8081]

knownRawCodes :: [Word16]
knownRawCodes = [0, 1, 2, 3, 8, 9, 10, 11, 13, 14, 16, 17, 18, 19, 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 61, 62, 63, 64, 65, 66, 67, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121]

code2RawCode :: Word16 -> Maybe Word16
code2RawCode =
    let m = M.fromList (zip knownCodes knownRawCodes)
    in \c -> M.lookup c m

