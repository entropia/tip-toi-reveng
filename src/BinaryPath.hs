{-# LANGUAGE RecordWildCards #-}
module BinaryPath where

import Types
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BC

-- This is used by Commands and Yaml handling, hence its own module 

defaultBinariesPath :: String
defaultBinariesPath = "binaries"

binariesWithPath :: String -> String -> Binaries -> [(String, BC.ByteString)]
binariesWithPath directory subdir binaries = map f binaries
  where
    f (desc, binary) = (printf "%s/%s/%s" directory subdir (BC.unpack desc), binary)

allBinariesWithPath :: String -> TipToiFile -> [(String, BC.ByteString)]
allBinariesWithPath directory (TipToiFile {..}) =
    binariesWithPath directory "games3201"  ttBinaryGames3201 ++
    binariesWithPath directory "games3202N" ttBinaryGames3202N ++
    binariesWithPath directory "games3203L" ttBinaryGames3203L ++
    binariesWithPath directory "main3201"   ttBinaryMain3201 ++
    binariesWithPath directory "main3202N"  ttBinaryMain3202N ++
    binariesWithPath directory "main3203L"  ttBinaryMain3203L
