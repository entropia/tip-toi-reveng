{-# LANGUAGE BangPatterns, TupleSections #-}

module OidCode (genRawPixels, genRawPNG, DPI(..), PixelSize(..)) where

import Data.Word
import Data.Bits
import Data.Functor
import Control.Monad
import Control.Monad.Writer.Strict
import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import Control.Applicative
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS


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


{-
oidSVG :: Int -> S.Svg
oidSVG code | code >= 4^8 = error $ printf "Code %d too large to draw" code
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
      where path = mkPath $ do
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

genSVGs :: String -> IO ()
genSVGs code_str = do
    codes <- parseRange code_str
    forM_ codes $ \c -> do
        let filename = printf "oid%d.svg" c
        printf "Writing %s...\n" filename
        genSVG c filename

genSVG :: Int -> FilePath -> IO ()
genSVG code filename = B.writeFile filename (renderSvg (oidSVG code))
-}

data DPI = D1200 | D600
data PixelSize = SinglePixel | DoublePixel

imageFromBlackPixels :: Int -> Int -> [(Int, Int)] -> Image PixelYA8
imageFromBlackPixels width height pixels = runST $ do
    i <- createMutableImage width height background
    forM_ pixels $ \(x,y) -> do
        writePixel i x y black
    freezeImage i
  where
    black =      PixelYA8 minBound maxBound
    background = PixelYA8 maxBound minBound

oidImage :: Int -> Int -> DPI -> PixelSize -> Word16 -> Image PixelYA8
oidImage w h dpi ps code =
    imageFromBlackPixels w h (tile f)
  where
    !dotsPerPoint | D1200 <- dpi = 12
                  |  D600 <- dpi =  6


    quart 8 = checksum code
    quart n = (code `div` 4^n) `mod` 4

    f = mconcat $ map position $
        zip (flip (,) <$> [3,2,1] <*> [3,2,1])
            [ value (quart n) | n <- [0..8] ] ++
        [ (p, plain) | p <- [(0,0), (1,0), (2,0), (3,0), (0,1), (0,3) ] ] ++
        [ ((0,2), special) ]

    plain | D1200 <- dpi, SinglePixel <- ps = coordsOfDots
                [ "           "
                , "           "
                , "           "
                , "           "
                , "           "
                , "    **     "
                , "    **     "
                , "           "
                , "           "
                , "           "
                , "           "
                , "           "
                ]
          | D1200 <- dpi, DoublePixel <- ps = coordsOfDots
                [ "           "
                , "           "
                , "           "
                , "           "
                , "   ****    "
                , "   ****    "
                , "   ****    "
                , "   ****    "
                , "           "
                , "           "
                , "           "
                , "           "
                ]
          | D600 <- dpi, SinglePixel <- ps  = coordsOfDots
                [ "      "
                , "      "
                , "  *   "
                , "      "
                , "      "
                , "      "
                ]
          | D600 <- dpi, DoublePixel <- ps  = coordsOfDots
                [ "      "
                , "      "
                , "  **  "
                , "  **  "
                , "      "
                , "      "
                ]


    s  | D1200 <- dpi = 2
       | D600  <- dpi = 1
    ss | D1200 <- dpi = 3
       | D600  <- dpi = 2
    value 0 = at ( s, s) plain
    value 1 = at (-s, s) plain
    value 2 = at (-s,-s) plain
    value 3 = at ( s,-s) plain
    special = at (ss,0)  plain

    position ((n,m), p) = at (n*dotsPerPoint, m*dotsPerPoint) p

    coordsOfDots :: [String] -> [(Int, Int)]
    coordsOfDots rows =
        [ (x,y)
        | (row, y) <- zip rows [0..]
        , (c, x)   <- zip row  [0..]
        , c == '*'
        ]

    -- Drawing combinators

    at (x, y) = map (\(x', y') -> (x + x', y + y'))
    tile f = concat [ at (x*4*dotsPerPoint, y*4*dotsPerPoint) f
                    | x <- [0..width-1], y <- [0..height-1]]
    width  = w `div` (4*dotsPerPoint)
    height = h `div` (4*dotsPerPoint)


-- Width and height in pixels
genRawPixels :: Int -> Int -> DPI -> PixelSize -> Word16 -> VU.Vector Word32
genRawPixels w h dpi ps code =
    -- All very shaky here, but it seems to work
    VS.convert $
    VS.unsafeCast $
    imageData $
    (promoteImage $ oidImage w h dpi ps code :: Image PixelRGBA8)


genRawPNG :: DPI -> PixelSize -> Word16 -> FilePath -> IO ()
genRawPNG dpi ps code filename = writePng filename (oidImage w h dpi ps code)
  where
    w = 100*dotsPerPoint*4
    h = 100*dotsPerPoint*4
    !dotsPerPoint | D1200 <- dpi = 12
                  |  D600 <- dpi =  6
