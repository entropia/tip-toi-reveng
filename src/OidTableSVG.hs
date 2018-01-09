module OidTableSVG where

import Data.Word
import qualified Data.ByteString.Lazy as LB
import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Text.Printf
import Control.Arrow ((***))
import Data.String
import Data.Complex

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.Utf8

import OidCode
import KnownCodes
import Types
import Utils

type Point = Complex Double

oidTableSvg :: Conf -> Bool -> String -> [(String, Word16)] -> LB.ByteString
oidTableSvg conf usePNG title entries
    | entriesPerPage < 1 = error "OID codes too large to fit on a single page"
    | otherwise = renderSvg $
        S.docTypeSvg  ! A.version (S.toValue "1.1")
                      ! A.width  (S.toValue (printf "%fmm" (a4w/mm) :: String))
                      ! A.height (S.toValue (printf "%fmm" (a4h/mm) :: String))
                      ! A.viewbox (S.toValue (printf "0 0 %f %f" a4w a4h :: String))
                      ! A.fontFamily (S.toValue "sans-serif")
                      $ do
    let patid d c | d == show c = d
                  | otherwise   = printf "%s-%d" d c

    -- Create patterns for the codes
    S.defs $ forM_ entries $ \(d,c) ->
        case code2RawCode c of
            Nothing -> return ()
            Just rc -> oidSVGPattern conf usePNG (patid d c) rc

    -- For SVG, we put all on one page (and exceed the page if it is too big)
    let chunks = [entries]
    let totalPages = length chunks

    forM_ (zip [1::Int ..] chunks) $ \(pageNum, thisPage) -> do
            S.text_ ! A.x (S.toValue (a4w / 2))
                    ! A.y (S.toValue (padTop + titleHeight))
                    ! A.textAnchor (S.toValue "middle")
                    ! A.stroke (S.toValue "black")
                    ! A.fontSize (S.toValue (printf "%f" (12*pt) :: String))
                    $ fromString title

            S.text_ ! A.x (S.toValue padLeft)
                    ! A.y (S.toValue (a4h - padBot))
                    ! A.textAnchor (S.toValue "left")
                    ! A.stroke (S.toValue "black")
                    ! A.fontSize (S.toValue (printf "%f" (8*pt) :: String))
                    $ fromString $ "Created by tttool-" ++ tttoolVersion


            forM_ (zip thisPage positions) $ \((d,c), x :+ y) -> do
                S.rect ! A.width (S.toValue imageWidth)
                       ! A.height (S.toValue imageHeight)
                       ! A.x (S.toValue x)
                       ! A.y (S.toValue y)
                       ! A.fill (S.toValue $ "url(#"++patid d c++")")

                S.text_ ! A.x (S.toValue x)
                        ! A.y (S.toValue (y + imageHeight + subtitleSep + subtitleHeight))
                        ! A.textAnchor (S.toValue "left")
                        ! A.stroke (S.toValue "black")
                        ! A.fontSize (S.toValue (printf "%f" (8*pt) :: String))
                        $ fromString d
  where
    -- Configure-dependent dimensions (all in pt)
    (imageWidth,imageHeight) = (*mm) *** (*mm) $ fromIntegral *** fromIntegral $cCodeDim conf

    -- Static dimensions (all in pt)

    -- Page paddings
    padTop, padLeft, padBot, padRight :: Double
    padTop   = 1*cm
    padBot   = 1*cm
    padLeft  = 2*cm
    padRight = 2*cm

    titleHeight  = 1*cm
    titleSep     = 0.5*cm
    footerHeight = 0.5*cm
    footerSep    = 0.5*cm

    imageSepH = 0.4*cm
    imageSepV = 0.2*cm

    subtitleHeight = 0.4*cm
    subtitleSep    = 0.2*cm

    -- Derived dimensions (all in pt)
    {-
    titleRect = Rectangle
        (padLeft          :+ (a4h - padTop - titleHeight))
        ((a4w - padRight) :+ (a4h - padTop))
    titleFont = Font (PDFFont Helvetica 12) black black

    footerRect = Rectangle
        (padLeft          :+ padBot)
        ((a4w - padRight) :+ (padBot + footerHeight))
    footerFont = Font (PDFFont Helvetica 8) black black

    bodyFont = Font (PDFFont Helvetica 8) black black
    -}

    bodyWidth  = a4w - padLeft - padRight
    bodyHeight = a4h - padTop - titleHeight - titleSep - footerSep - footerHeight - padBot


    positions = map (+(padLeft :+ (padTop + titleHeight + titleSep))) $
        calcPositions bodyWidth  bodyHeight
                      imageWidth (imageHeight + subtitleSep + subtitleHeight)
                      imageSepH  imageSepV
    entriesPerPage = length positions


    -- Derived dimensions (all in pixels)
    imageWidthPx = floor (imageWidth * px)
    imageHeightPx = floor (imageHeight * px)

    -- config-dependent conversion factors
    px :: Double
    px = fromIntegral (cDPI conf) / 72


    {-
    -- Makes sure the given point is at a coordinate that is a multiple
    -- of an pixel
    align :: Point -> Point
    align pos = alignToPx (realPart pos) :+ (a4h - alignToPx (a4h - imagPart pos))

    -- Makes sure the given distance is an interal mulitple of a pixel
    alignToPx :: Double -> Double
    alignToPx x = fromIntegral (floor (x * px)) / px
    -}

calcPositions
    :: Double -- ^ total width
    -> Double -- ^ total height
    -> Double -- ^ entry width
    -> Double -- ^ entry height
    -> Double -- ^ pad width
    -> Double -- ^ pad height
    -> [Point]
calcPositions tw th ew eh pw ph = [ x :+ ({-(th - -} y) | y <- ys , x <- xs]
  where
    xs = [0,ew+pw..tw-ew]
    ys = [0,eh+ph..th-eh]

pt :: Double
pt = 48/2.83465

-- Conversation factor
cm :: Double
cm = 10 * mm

mm :: Double
mm = 2.83465 * pt

-- A4 dimensions
a4w, a4h :: Double
a4w = 595 * pt
a4h = 842 * pt
