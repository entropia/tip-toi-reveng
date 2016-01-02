module OidTable where

import Data.Word
import qualified Data.ByteString.Lazy as LB
import Graphics.PDF
import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Data.List.Split
import Text.Printf
import Control.Arrow ((***))
import Codec.Compression.Zlib

import OidCode
import KnownCodes
import Utils
import Types


-- IO technically unnecessary: https://github.com/alpheccar/HPDF/issues/7

oidTable :: Conf -> String -> [(String, Word16)] -> LB.ByteString
oidTable conf title entries | entriesPerPage < 1 = error "OID codes too large to fit on a single page"
                            | otherwise = pdfByteString docInfo a4rect $ do
    -- Replace codes by images
    entries' <- forM entries $ \(d,rc) ->
        case code2RawCode rc of
            Nothing -> return (d, Nothing)
            Just c -> do
                image <- createPDFRawImageFromByteString imageWidthPx imageHeightPx False FlateDecode $
                    compressWith defaultCompressParams { compressLevel = defaultCompression } $
                    genRawPixels imageWidthPx imageHeightPx (cDPI conf) (cPixelSize conf) $
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
                paragraph $ txt $ "Created by tttool-" ++ tttoolVersion

            displayFormattedText footerRect NormalParagraph footerFont $ do
                setJustification RightJustification
                paragraph $ txt $ printf "%d/%d" pageNum totalPages

            forM_ (zip thisPage positions) $ \((e,mbi),p) -> do
                withNewContext $ do
                    applyMatrix $ translate  p
                    forM_ mbi $ \i -> withNewContext $ do
                        applyMatrix $ translate  (0 :+ (-imageHeight))
                        applyMatrix $ scale (1/px) (1/px)
                        drawXObject i
                    withNewContext $ do
                        applyMatrix $ translate  (0 :+ (-imageHeight - subtitleSep))
                        let fontRect = Rectangle (0 :+ (-subtitleHeight)) (imageWidth :+ 0)
                        addShape fontRect
                        setAsClipPath
                        displayFormattedText fontRect NormalParagraph bodyFont $ do
                            paragraph $ txt e
  where
    docInfo = standardDocInfo
        { author=toPDFString $ "tttool-" ++ tttoolVersion
        , compressed = False
        }

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
    titleRect = Rectangle
        (padLeft          :+ (a4h - padTop - titleHeight))
        ((a4w - padRight) :+ (a4h - padTop))
    titleFont = Font (PDFFont Helvetica 12) black black

    footerRect = Rectangle
        (padLeft          :+ padBot)
        ((a4w - padRight) :+ (padBot + footerHeight))
    footerFont = Font (PDFFont Helvetica 8) black black

    bodyFont = Font (PDFFont Helvetica 8) black black

    bodyWidth  = a4w - padLeft - padRight
    bodyHeight = a4h - padTop - titleHeight - titleSep - footerSep - footerHeight - padBot

    positions = map (+(padLeft :+ (padBot + footerHeight + footerSep))) $
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
    xs = [0,ew+pw..tw-ew]
    ys = [0,eh+ph..th-eh]

-- Conversation factor
cm :: Double
cm = 28.3465

mm :: Double
mm = 2.83465

-- A4 dimensions
a4w, a4h :: Double
a4w = 595
a4h = 842

a4rect :: PDFRect
a4rect = PDFRect 0 0 595 842

