module Cypher (cypher) where

import Data.Bits
import qualified Data.ByteString.Lazy as B

import Types

cypher :: Word8 -> B.ByteString -> B.ByteString
cypher x = B.map go
    where go 0 = 0
          go 255 = 255
          go n | n == x    = n
               | n == xor x 255 = n
               | otherwise = xor x n

