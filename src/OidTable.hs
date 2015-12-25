module OidTable where

import Data.Word
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Unboxed as V
import Graphics.PDF
import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Data.List.Split
import Text.Printf
import Data.Version (showVersion)

import OidCode
import KnownCodes
import Paths_tttool


-- IO technically unnecessary: https://github.com/alpheccar/HPDF/issues/7

oidTable :: String -> [(String, Word16)] -> IO LB.ByteString
oidTable title entries = pdfByteString docInfo a4rect $ do

    -- Replace codes by imagesj
    entries' <- forM entries $ \(d,rc) ->
        case code2RawCode rc of
            Nothing -> return (d, Nothing)
            Just c -> do
                image <- createPDFRawImage' imageSizePixels imageSizePixels False $
                    genRawPixels imageSizePixels imageSizePixels 1200 1 $
                    c
                return (d, Just image)

    let chunks = chunksOf entriesPerPage entries'
    let totalPages = length chunks

    forM_ (zip [1::Int ..] chunks) $ \(pageNum, thisPage) -> do
        page <- addPage Nothing

        drawWithPage page $ do
            displayFormattedText titleRect NormalParagraph titleFont $ do
                setJustification Centered
                paragraph $ txt title

            displayFormattedText footerRect NormalParagraph footerFont $ do
                setJustification LeftJustification
                paragraph $ txt $ "Created by tttool-" ++ showVersion version

            displayFormattedText footerRect NormalParagraph footerFont $ do
                setJustification RightJustification
                paragraph $ txt $ printf "%d/%d" pageNum totalPages

            forM_ (zip thisPage positions) $ \((e,mbi),p) -> do
                withNewContext $ do
                    applyMatrix $ translate  p
                    forM_ mbi $ \i -> withNewContext $ do
                        applyMatrix $ translate  (0 :+ (-imageSize))
                        applyMatrix $ scale (1/px) (1/px)
                        drawXObject i
                    withNewContext $ do
                        applyMatrix $ translate  (0 :+ (-imageSize - 0.2*cm))
                        let fontRect = Rectangle (0 :+ (-1*cm)) (imageSize :+ 0)
                        addShape fontRect
                        -- setAsClipPath
                        displayFormattedText fontRect
                                NormalParagraph bodyFont $ do
                            paragraph $ txt e
  where
    docInfo = standardDocInfo { author=toPDFString "tttool", compressed = True}

    positions = map (+(2*cm :+ 2*cm)) $
        calcPositions
            (a4w - 4*cm) (a4h - 6*cm)
            (imageSize)  (imageSize + 1*cm)
            (0.4*cm)     (0.4*cm)
    entriesPerPage = length positions

calcPositions
    :: Double -- ^ total width
    -> Double -- ^ total height
    -> Double -- ^ entry width
    -> Double -- ^ entry height
    -> Double -- ^ pad width
    -> Double -- ^ pad height
    -> [Point]
calcPositions tw th ew eh pw ph = [ x :+ (th - y) | y <- ys , x <- xs]
  where
    xs = takeWhile (<= tw - ew) [0,ew+pw..]
    ys = takeWhile (<= th - eh) [0,eh+ph..]


-- in pt
imageSize = 3*cm
imageSizePixels = floor (imageSize * px)

bodyContainer :: Container ps  s
bodyContainer = mkContainer (2*cm) (a4h - 4*cm) (a4w - 4*cm) (a4h - 6*cm) 0
bodyFont = Font (PDFFont Helvetica 12) black black

titleRect = Rectangle (2*cm :+ (a4h - 3*cm)) ((a4w - 2*cm) :+ (a4h - 2*cm))
titleFont = Font (PDFFont Helvetica 16) black black

footerRect = Rectangle (2*cm :+ 1*cm) ((a4w - 2*cm) :+ 2*cm)
footerFont = Font (PDFFont Helvetica 10) black black

-- More sensible types (see https://github.com/alpheccar/HPDF/issues/8)
createPDFRawImage' :: Int -> Int -> Bool -> V.Vector Word32 -> PDF (PDFReference RawImage)
createPDFRawImage' w h i v = createPDFRawImage (fromIntegral w) (fromIntegral h) i v

-- Conversation factor
cm :: Double
cm = 28.3465

px :: Double
px = 1200/72

-- A4 dimensions
a4w, a4h :: Double
a4w = 595
a4h = 842

a4rect :: PDFRect
a4rect = PDFRect 0 0 595 842

