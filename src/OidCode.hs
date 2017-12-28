{-# LANGUAGE BangPatterns, TupleSections, FlexibleContexts #-}

module OidCode (genRawPixels, genRawPNG, genRawSVG, tilePixelSize, DPI(..), PixelSize(..)) where

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

import qualified Text.Blaze.Svg as S
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.Utf8
import qualified Data.ByteString.Lazy as B (writeFile)
import Control.Arrow


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


oidSVG :: Word16 -> S.Svg
oidSVG code = S.docTypeSvg ! A.version (S.toValue "1.1")
                           ! A.width (S.toValue "1mm")
                           ! A.height (S.toValue "1mm")
                           ! A.viewbox (S.toValue "0 0 48 48") $ do
    S.defs pattern
    S.rect ! A.width (S.toValue "48") ! A.height (S.toValue "48")
           ! A.fill (S.toValue $ "url(#"++patid++")")
  where
    quart 8 = checksum code
    quart n = (code `div` 4^n) `mod` 4
    patid = "pat-" ++ show code

    pattern = S.pattern ! A.width (S.toValue "48")
                        ! A.height (S.toValue "48")
                        ! A.id_ (S.toValue patid)
                        ! A.patternunits (S.toValue "userSpaceOnUse") $ S.g (f (0,0))
    f = mconcat $ map position $
        zip (flip (,) <$> [3,2,1] <*> [3,2,1])
            [ value (quart n) | n <- [0..8] ] ++
        [ (p, plain) | p <- [(0,0), (1,0), (2,0), (3,0), (0,1), (0,3) ] ] ++
        [ ((0,2), special) ]

    -- pixel = S.rect ! A.width (S.toValue "2") ! A.height (S.toValue "2") ! pos (7,7)
    pixel (x,y) = S.path ! A.d path
      where path = S.mkPath $ do
                S.m (x+5) (y+5)
                S.hr 2
                S.vr 2
                S.hr (-2)
                S.z

    plain = pixel
    value 0 = at (2,2)   plain
    value 1 = at (-2,2)  plain
    value 2 = at (-2,-2) plain
    value 3 = at (2,-2)  plain
    special = at (3,0)   plain

    position ((n,m), p) = at (n*12, m*12) p

    -- Drawing combinators
    at (x, y) f = f . ((+x) *** (+y))

genRawSVG :: Word16 -> FilePath -> IO ()
genRawSVG code filename = B.writeFile filename (renderSvg (oidSVG code))

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

-- | Size of one tile, in pixels
tilePixelSize :: DPI -> PixelSize -> Int
tilePixelSize dpi _ps = width
  where
    spacePerPoint = dpi `div2` 100
    width  = 4*spacePerPoint

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


genRawPNG :: Int -> Int -> DPI -> PixelSize -> String -> Word16 -> FilePath -> IO ()
genRawPNG w h dpi ps title code filename =
    B.writeFile filename $
    encodePngWithMetadata metadata $
    (oidImage w h dpi ps code :: Image PixelYA8)
  where
    metadata = mconcat
        [ singleton DpiX (fromIntegral dpi)
        , singleton DpiY (fromIntegral dpi)
        , singleton Title title
        , singleton Software $ "tttool " ++ tttoolVersion
        ]
