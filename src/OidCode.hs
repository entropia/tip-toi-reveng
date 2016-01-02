{-# LANGUAGE BangPatterns, TupleSections, FlexibleContexts #-}

module OidCode (genRawPixels, genRawPNG, DPI(..), PixelSize(..)) where

import Data.Word
import Data.Bits
import Data.Functor
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import Control.Monad
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Metadata
import Control.Monad.ST
import Control.Applicative
import Data.Vector.Storable.ByteString

import Utils

-- Image generation

checksum :: Word16 -> Word16
checksum dec = c3
  where
    c1  =       (((dec >> 2) ^ (dec >> 8) ^ (dec >> 12) ^ (dec >> 14)) & 0x01) << 1
    c2 = c1 .|. (((dec) ^ (dec >> 4) ^ (dec >>  6) ^ (dec >> 10)) & 0x01)
    c3 = c2  ^  0x02

    (>>) = shiftR
    (<<) = shiftL
    (^) = xor
    (&) = (.&.)


type DPI = Int
type PixelSize = Int

imageFromBlackPixels :: ColorConvertible PixelYA8 p => Int -> Int -> [(Int, Int)] -> Image p
imageFromBlackPixels width height pixels = runST $ do
    i <- createMutableImage width height background
    forM_ pixels $ \(x,y) -> do
        writePixel i x y black
    freezeImage i
  where
    black =      promotePixel $ PixelYA8 minBound maxBound
    background = promotePixel $ PixelYA8 maxBound minBound

-- | Renders a single OID Image, returns its dimensions and the black pixels therein
singeOidImage :: DPI -> PixelSize -> Word16 -> ((Int, Int), [(Int, Int)])
singeOidImage dpi ps code = ((width, height), pixels)
  where
    spacePerPoint = dpi `div2` 100
    width  = 4*spacePerPoint
    height = 4*spacePerPoint

    pixels = mconcat $ map position $
        zip (flip (,) <$> [3,2,1] <*> [3,2,1])
            [ value (quart n) | n <- [0..8] ] ++
        [ (p, centeredDot) | p <- [(0,0), (1,0), (2,0), (3,0), (0,1), (0,3) ] ] ++
        [ ((0,2), special) ]


    quart 8 = checksum code
    quart n = (code `div` 4^n) `mod` 4


    dot = [(x,y) | x <- [1..ps], y <- [1..ps]]

    centeredDot | xshift < 0 || yshift < 0 = error "Dots too large. Try a smaller pixel size"
                | otherwise = at (xshift, yshift) dot
      where xshift = (spacePerPoint - ps) `div` 2 - 1
            yshift = (spacePerPoint - ps) `div` 2 - 1

    -- | how many pixels to shift dots horizontally
    s  = dpi `div2` 600
    -- | how many pixels to shift the special dot horizontally
    ss = dpi `div2` 400

    value 0 = at ( s, s) centeredDot
    value 1 = at (-s, s) centeredDot
    value 2 = at (-s,-s) centeredDot
    value 3 = at ( s,-s) centeredDot
    special = at (ss,0)  centeredDot

    position ((n,m), p) = at (n*spacePerPoint, m*spacePerPoint) p

    -- Drawing combinators
    at (x, y) = map (\(x', y') -> (x + x', y + y'))

    -- integer division rounded up
    x `div2` y = ((x-1) `div` y) + 1

oidImage :: ColorConvertible PixelYA8 p => Int -> Int -> DPI -> PixelSize -> Word16 -> Image p
oidImage w h dpi ps code =
    imageFromBlackPixels w h tiledPixels
  where
    ((cw,ch), pixels) = singeOidImage dpi ps code

    tiledPixels =
        [ (x',y')
        | (x,y) <- pixels
        , x' <- [x,x + cw..w-1]
        , y' <- [y,y + ch..h-1]
        ]

-- Width and height in pixels
genRawPixels :: Int -> Int -> DPI -> PixelSize -> Word16 -> B.ByteString
genRawPixels w h dpi ps code =
    -- All very shaky here, but it seems to work
    B.fromStrict $
    vectorToByteString $
    imageData $
    (oidImage w h dpi ps code :: Image PixelRGB8)


genRawPNG :: Int -> Int -> DPI -> PixelSize -> Word16 -> FilePath -> IO ()
genRawPNG w h dpi ps code filename =
    B.writeFile filename $
    encodePngWithMetadata metadata $
    (oidImage w h dpi ps code :: Image PixelYA8)
  where
    metadata = mconcat
        [ singleton DpiX (fromIntegral dpi)
        , singleton DpiY (fromIntegral dpi)
        , singleton Title $ "Tiptoi OID Code " ++ show code
        , singleton Software $ "tttool " ++ tttoolVersion
        ]
