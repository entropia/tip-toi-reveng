{-# LANGUAGE CPP, RecordWildCards, TupleSections #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Binary.Builder as Br
import qualified Data.Binary.Get as G
import Text.Printf
import Data.Bits
import Data.List
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Ord
import Control.Monad
import System.Directory
import Numeric (readHex)
import qualified Data.Map as M
import Data.Foldable (for_)
import Data.Version

import Types
import Constants
import KnownCodes
import GMEParser
import GMEWriter
import GMERun
import PrettyPrint
import RangeParser
import OidCode
import Utils
import TipToiYaml
import Lint

import Paths_tttool


-- Main commands

dumpAudioTo :: FilePath -> FilePath -> IO ()
dumpAudioTo directory file = do
    (tt,_) <- parseTipToiFile <$> B.readFile file

    printf "Audio Table entries: %d\n" (length (ttAudioFiles tt))

    createDirectoryIfMissing False directory
    forMn_ (ttAudioFiles tt) $ \n audio -> do
        let audiotype = maybe "raw" snd $ find (\(m,t) -> m `B.isPrefixOf` audio) fileMagics
        let filename = printf "%s/%s_%d.%s" directory (takeBaseName file) n audiotype
        if B.null audio
        then do
            printf "Skipping empty file %s...\n" filename
        else do
            B.writeFile filename audio
            printf "Dumped sample %d as %s\n" n filename

dumpBinariesTo :: FilePath -> FilePath -> IO ()
dumpBinariesTo directory file = do
    (TipToiFile {..},_) <- parseTipToiFile <$> B.readFile file

    let binaries =
            map (1,) ttBinaries1 ++
            map (2,) ttBinaries2 ++
            map (3,) ttBinaries3 ++
            map (4,) ttBinaries4

    printf "Binary Table entries: %d\n" (length binaries)

    createDirectoryIfMissing False directory
    forM_ binaries $ \(n,(desc,binary)) -> do
        let filename = printf "%s/%d_%s" directory (n::Int) (BC.unpack desc)
        if B.null binary
        then do
            printf "Skipping empty file %s...\n" filename
        else do
            B.writeFile filename binary
            printf "Dumped binary %s from block %d as %s\n" (BC.unpack desc) n filename

dumpScripts :: Transscript -> Bool -> Maybe Int -> FilePath -> IO ()
dumpScripts t raw sel file = do
    bytes <- B.readFile file
    let (tt,_) = parseTipToiFile bytes
        st' | Just n <- sel = filter ((== fromIntegral n) . fst) (ttScripts tt)
            | otherwise     = ttScripts tt

    forM_ st' $ \(i, ms) -> case ms of
        Nothing -> do
            printf "Script for OID %d: Disabled\n" i
        Just lines -> do
            printf "Script for OID %d:\n" i
            forM_ lines $ \line -> do
                if raw then printf "%s\n"     (lineHex bytes line)
                       else printf "    %s\n" (ppLine t line)


dumpInfo :: Transscript -> FilePath -> IO ()
dumpInfo t file = do
    (TipToiFile {..},_) <- parseTipToiFile <$> B.readFile file
    let st = ttScripts

    printf "Product ID: %d\n" ttProductId
    printf "Raw XOR value: 0x%08X\n" ttRawXor
    printf "Magic XOR value: 0x%02X\n" ttAudioXor
    printf "Comment: %s\n" (BC.unpack ttComment)
    printf "Date: %s\n" (BC.unpack ttDate)
    printf "Number of registers: %d\n" (length ttInitialRegs)
    printf "Initial registers: %s\n" (show ttInitialRegs)
    printf "Initial sounds: %s\n" (ppPlayListList t ttWelcome)
    printf "Scripts for OIDs from %d to %d; %d/%d are disabled.\n"
        (fst (head st)) (fst (last st))
        (length (filter (isNothing . snd) st)) (length st)
    printf "Audio table entries: %d\n" (length ttAudioFiles)
    when ttAudioFilesDoubles $ printf "Audio table repeated twice\n"
    printf "Binary tables entries: %d/%d/%d/%d\n"
        (length ttBinaries1)
        (length ttBinaries2)
        (length ttBinaries3)
        (length ttBinaries4)
    for_ ttSpecialOIDs $ \(oid1, oid2) ->
        printf "Speical OIDs: %d, %d\n" oid1 oid2
    printf "Checksum found 0x%08X, calculated 0x%08X\n" ttChecksum ttChecksumCalc

lint :: FilePath -> IO ()
lint file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    lintTipToi tt segments

play :: Transscript -> FilePath -> IO ()
play t file = do
    (cm,tt) <-
        if ".yaml" `isSuffixOf` file
        then do
            (tty, extraCodeMap) <- readTipToiYaml file
            (tt, codeMap) <- ttYaml2tt (takeDirectory file) tty extraCodeMap
            return (codeMap, tt)
        else do
            (tt,_) <- parseTipToiFile <$> B.readFile file
            return (M.empty, tt)
    playTipToi cm t tt

segments :: FilePath -> IO ()
segments file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    mapM_ printSegment segments

printSegment (o,l,desc) = printf "At 0x%08X Size %8d: %s\n" o l (ppDesc desc)

explain :: FilePath -> IO ()
explain file = do
    bytes <- B.readFile file
    let (tt,segments) = parseTipToiFile bytes
    forM_ (addHoles segments) $ \e -> case e of
        Left (o,l) -> do
            printSegment (o,l,["-- unknown --"])
            printExtract bytes o l
            putStrLn ""
        Right ss@((o,l,_):_) -> do
            mapM_ printSegment ss
            printExtract bytes o l
            putStrLn ""

printExtract :: B.ByteString -> Offset -> Word32 -> IO ()
printExtract b o l = do
    let o1 = o .&. 0xFFFFFFF0
    lim_forM_ [o1, o1+0x10 .. (o + l-1)] $ \s -> do
        let s' = max o s
        let d  = fromIntegral s' - fromIntegral s
        let l' = (min (o + l) (s + 0x10)) - s'
        printf "   0x%08X: %s%s\n"
            s
            (replicate (d*3) ' ')
            (prettyHex (extract s' l' b))
  where
    lim_forM_ l act
        = if length l > 30
          then do act (head l)
                  printf "   (skipping %d lines)\n" (length l - 2) :: IO ()
                  act (last l)
          else do forM_ l act

findPosition :: Integer -> FilePath -> IO ()
findPosition pos' file = do
    (tt,segments) <- parseTipToiFile <$> B.readFile file
    case find (\(o,l,_) -> pos >= o && pos < o + l) segments of
        Just s -> do
            printf "Offset 0x%08X is part of this segment:\n" pos
            printSegment s
        Nothing -> do
            let before = filter (\(o,l,_) -> pos >= o + l) segments
                after = filter (\(o,l,_) -> pos < o) segments
                printBefore | null before = printf "(nothing before)\n"
                            | otherwise   = printSegment (maximumBy (comparing (\(o,l,_) -> o+l)) before)
                printAfter  | null after  = printf "(nothing after)\n"
                            | otherwise   = printSegment (minimumBy (comparing (\(o,l,_) -> o)) after)
            printf "Offset %08X not found. It lies between these two segments:\n" pos
            printBefore
            printAfter

    where
    pos = fromIntegral pos'


-- returns a list of segments in case of overlap
addHoles :: [Segment] -> [Either (Offset, Word32) [Segment]]
addHoles = go 0
   where go at [] = []
         go at ss@((o,l,d):_)
            | at /= o -- a hole
            = Left (at, o-at) : go o ss
            | otherwise -- no hole
            = let (this, others) = span (\(o',l',_) -> o == o' && l == l') ss
              in Right this : go (o + l) others

unknown_segments :: FilePath -> IO ()
unknown_segments file = do
    bytes <- B.readFile file
    let (_,segments) = parseTipToiFile bytes
    let unknown_segments =
            filter (\(o,l) -> not
                (l == 2 && G.runGet (G.skip (fromIntegral o) >> G.getWord16le) bytes == 0)) $
            lefts $ addHoles $ segments
    printf "Unknown file segments: %d (%d bytes total)\n"
        (length unknown_segments) (sum (map snd unknown_segments))
    forM_ unknown_segments $ \(o,l) ->
        printf "   Offset: %08X to %08X (%d bytes)\n" o (o+l) l


withEachFile :: (FilePath -> IO ()) -> [FilePath] -> IO ()
withEachFile _ [] = main' undefined []
withEachFile a [f] = a f 
withEachFile a fs = forM_ fs $ \f -> do 
    printf "%s:\n" f 
    a f


dumpGames :: Transscript -> FilePath -> IO ()
dumpGames t file = do
    bytes <- B.readFile file
    let (tt,_) = parseTipToiFile bytes
    forMn_ (ttGames tt) $ \n g -> do
        printf "Game %d:\n" n
        printf "%s\n" (ppGame t g)

writeTipToi :: FilePath -> TipToiFile -> IO ()
writeTipToi out tt = do
    let bytes = writeTipToiFile tt
    let checksum = B.foldl' (\s b -> fromIntegral b + s) 0 bytes
    B.writeFile out $ Br.toLazyByteString $
        Br.fromLazyByteString bytes `Br.append` Br.putWord32le checksum

rewrite :: FilePath -> FilePath -> IO ()
rewrite inf out = do
    (tt,_) <- parseTipToiFile <$> B.readFile inf
    writeTipToi out tt

export :: FilePath -> FilePath -> IO ()
export inf out = do
    (tt,_) <- parseTipToiFile <$> B.readFile inf
    let tty = tt2ttYaml (printf "media/%s_%%s" (takeBaseName inf)) tt
    ex <- doesFileExist out
    if ex
        then printf "File \"%s\" does already exist. Please remove it first\nif you want to export \"%s\" again.\n" out inf >> exitFailure
        else writeTipToiYaml out tty


assemble :: FilePath -> FilePath -> IO ()
assemble inf out = do
    (tty, codeMap) <- readTipToiYaml inf
    (tt, totalMap) <- ttYaml2tt (takeDirectory inf) tty codeMap
    writeTipToiCodeYaml inf tty codeMap totalMap
    writeTipToi out tt


genPNGs :: DPI -> PixelSize -> String -> IO ()
genPNGs dpi ps arg = do
    ex <- doesFileExist arg
    if ex then genPNGsForFile dpi ps arg
          else genPNGsForCodes dpi ps arg

genPNGsForFile :: DPI -> PixelSize -> FilePath -> IO ()
genPNGsForFile dpi ps inf = do
    (tty, codeMap) <- readTipToiYaml inf
    (tt, totalMap) <- ttYaml2tt (takeDirectory inf) tty codeMap
    let codes = ("START", fromIntegral (ttProductId tt)) : M.toList totalMap
    forM_  codes $ \(s,c) -> do
        genPNG dpi ps c $ printf "oid-%d-%s.png" (ttProductId tt) s

genPNGsForCodes :: DPI -> PixelSize -> String -> IO ()
genPNGsForCodes dpi ps code_str = do
    codes <- parseRange code_str
    forM_ codes $ \c -> do
        genPNG dpi ps c $ printf "oid-%d.png" c

genPNG :: DPI -> PixelSize -> Word16 -> String -> IO ()
genPNG dpi ps c filename =
    case code2RawCode c of
        Nothing -> printf "Skipping %s, code %d not known.\n" filename c
        Just r -> do
            printf "Writing %s.. (Code %d, raw code %d)\n" filename c r
            genRawPNG dpi ps r filename


genPNGsForRawCodes :: DPI -> PixelSize -> String -> IO ()
genPNGsForRawCodes dpi ps code_str = do
    codes <- parseRange code_str
    forM_ codes $ \r -> do
        let filename = printf "oid-raw-%d.png" r
        printf "Writing %s... (raw code %d)\n" filename r
        genRawPNG dpi ps r filename


-- The main function


readTransscriptFile :: FilePath -> IO Transscript
readTransscriptFile transcriptfile_ = do
    file <- readFile transcriptfile_
    return $ M.fromList
        [ (idx, string)
        | l <- lines file
        , (idxstr:string:_) <- return $ wordsWhen (';'==) l
        , Just idx <- return $ readMaybe idxstr
        ]

-- Avoiding dependencies, using code from http://stackoverflow.com/a/4981265/946226
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main' t ("-t":transscript:args) =
    do t2 <- readTransscriptFile transscript
       main' (t `M.union` t2) args

main' t ("export": inf : [] )       = main' t ("export":inf: dropExtension inf <.> "yaml":[])
main' t ("assemble": inf : [] )     = main' t ("assemble":inf: dropExtension inf <.> "gme":[])

main' t ("info": files)             = withEachFile (dumpInfo t) files
main' t ("media": "-d": dir: files) = withEachFile (dumpAudioTo dir) files
main' t ("media": files)            = withEachFile (dumpAudioTo "media") files
main' t ("scripts": files)          = withEachFile (dumpScripts t False Nothing) files
main' t ("script":  file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t False (Just int) file
main' t ("raw-scripts": files)      = withEachFile (dumpScripts t True Nothing) files
main' t ("raw-script": file : n:[])
    | Just int <- readMaybe n       =              dumpScripts t True (Just int) file
main' t ("binaries": "-d": dir: files) = withEachFile (dumpBinariesTo dir) files
main' t ("binaries": files)            = withEachFile (dumpBinariesTo "binaries") files
main' t ("games": files)            = withEachFile (dumpGames t) files
main' t ("lint": files)             = withEachFile lint files
main' t ("segments": files)         = withEachFile segments files
main' t ("segment": file : n :[])
    | Just int <- readMaybe n       =              findPosition int file
    | [(int,[])] <- readHex n       =              findPosition int file
main' t ("holes": files)            = withEachFile unknown_segments files
main' t ("explain": files)          = withEachFile explain files
main' t ("play": file : [])         =              play t file
main' t ("rewrite": inf : out: [])  =              rewrite inf out
main' t ("export": inf : out: [] )  =              export inf out
main' t ("assemble": inf : out: []) =              assemble inf out
main' t ("oid-code": "-d" : "600" : codes@(_:_))
                                    =              genPNGs D600 SinglePixel (unwords codes)
main' t ("oid-code": "-d" : "1200" : codes@(_:_))
                                    =              genPNGs D1200 SinglePixel (unwords codes)
main' t ("oid-code": "-d" : "600d" : codes@(_:_))
                                    =              genPNGs D600 DoublePixel (unwords codes)
main' t ("oid-code": "-d" : "1200d" : codes@(_:_))
                                    =              genPNGs D1200 DoublePixel (unwords codes)
main' t ("oid-code": "-d" : _)      = do
    putStrLn $ "The parameter to -d has to be 600, 1200, 600d or 1200d"
    exitFailure
main' t ("oid-code": codes@(_:_))   =              genPNGs D1200 SinglePixel (unwords codes)
main' t ("raw-oid-code": "-d" : "600" : codes@(_:_))
                                    =              genPNGsForRawCodes D600 SinglePixel (unwords codes)
main' t ("raw-oid-code": "-d" : "1200" : codes@(_:_))
                                    =              genPNGsForRawCodes D1200 SinglePixel (unwords codes)
main' t ("raw-oid-code": "-d" : "600d" : codes@(_:_))
                                    =              genPNGsForRawCodes D600 DoublePixel (unwords codes)
main' t ("raw-oid-code": "-d" : "1200d" : codes@(_:_))
                                    =              genPNGsForRawCodes D1200 DoublePixel (unwords codes)
main' t ("raw-oid-code": "-d" : _)  = do
    putStrLn $ "The parameter to -d has to be 600, 1200, 600d or 1200d"
    exitFailure
main' t ("raw-oid-code": codes@(_:_)) =            genPNGsForRawCodes D1200 SinglePixel (unwords codes)
main' _ _ = do
    prg <- getProgName
    putStrLn $ "This is the Tiptoi toolkit, version " ++ showVersion version
    putStrLn $ ""
    putStrLn $ "Usage: " ++ prg ++ " [options] command"
    putStrLn $ ""
    putStrLn $ "Options:"
    putStrLn $ "    -t <transcriptfile>"
    putStrLn $ "       in the screen output, replaces media file indices by a transscript"
    putStrLn $ ""
    putStrLn $ "Commands:"
    putStrLn $ "    info <file.gme>..."
    putStrLn $ "       general information"
    putStrLn $ "    media [-d dir] <file.gme>..."
    putStrLn $ "       dumps all audio samples to the given directory (default: media/)"
    putStrLn $ "    scripts <file.gme>..."
    putStrLn $ "       prints the decoded scripts for each OID"
    putStrLn $ "    script <file.gme> <n>"
    putStrLn $ "       prints the decoded scripts for the given OID"
    putStrLn $ "    raw-scripts <file.gme>..."
    putStrLn $ "       prints the scripts for each OID, in their raw form"
    putStrLn $ "    raw-script <file.gme> <n>"
    putStrLn $ "       prints the scripts for the given OID, in their raw form"
    putStrLn $ "    binaries [-d dir] <file.gme>..."
    putStrLn $ "       dumps all binaries to the given directory (default: binaries/)"
    putStrLn $ "    games <file.gme>..."
    putStrLn $ "       prints the decoded games"
    putStrLn $ "    lint <file.gme>"
    putStrLn $ "       checks for errors in the file or in this program"
    putStrLn $ "    segments <file.gme>..."
    putStrLn $ "       lists all known parts of the file, with description."
    putStrLn $ "    segment <file.gme> <pos>"
    putStrLn $ "       which segment contains the given position."
    putStrLn $ "    holes <file.gme>..."
    putStrLn $ "       lists all unknown parts of the file."
    putStrLn $ "    explain <file.gme>..."
    putStrLn $ "       lists all parts of the file, with description and hexdump."
    putStrLn $ "    play <file.gme or file.yaml>"
    putStrLn $ "       interactively play: Enter OIDs, and see what happens."
    putStrLn $ "    rewrite <infile.gme> <outfile.gme>"
    putStrLn $ "       parses the file and serializes it again (for debugging)."
    putStrLn $ "    export <infile.gme> [<outfile.yaml>]"
    putStrLn $ "       dumps the file in the human-readable yaml format"
    putStrLn $ "    assemble <infile.yaml> <outfile.gme>"
    putStrLn $ "       creates a gme file from the given source"
    putStrLn $ "    oid-code [-d DPI] <codes>"
    putStrLn $ "       creates a PNG file for each given code"
    putStrLn $ "       scale this to 10cm×10cm"
    putStrLn $ "       By default, it creates a 1200 dpi image. With -d 600, you"
    putStrLn $ "       obtain a 600 dpi image. With -d 600d resp. 1200d you can"
    putStrLn $ "       double the size of the pixel."
    putStrLn $ "       <codes> can be a range, e.g. 1,3,1000-1085."
    putStrLn $ "       Uses oid-<code>.png as the file name."
    putStrLn $ "    oid-code [-d DPI] <infile.yaml>"
    putStrLn $ "       Like above, but creates one file for each code in the yaml file."
    putStrLn $ "       Uses oid-<product-id>-<scriptname or code>.png as the file name."
    putStrLn $ "    raw-oid-code [-d DPI] <raw codes>"
    putStrLn $ "       creates a PNG file with the given \"raw code\". Usually not needed."
    putStrLn $ "       Uses oid-raw-<code>.png as the file name."
    exitFailure

main = getArgs >>= (main' M.empty)

