import Options.Applicative

import System.FilePath
import Numeric
import Data.List (intercalate)
import Options.Applicative.Help.Chunk
import Data.Maybe
import Data.Char (toLower)

import Types
import RangeParser
import Commands
import Utils
import BinaryPath

-- Parameter parsing

optionParser :: ParserInfo (IO ())
optionParser =
    info (helper <*> (conf <**> cmd)) $ mconcat
    [ progDesc $ "tttool-" ++ tttoolVersion ++ " -- The swiss army knife for the Tiptoi hacker"
    , footerDoc foot
    , fullDesc
    ]
  where
    foot = unChunk $ vsepChunks
        [ paragraph "Please run \"tttool COMMAND --help\" for information on the particular command."
        ]

    conf = pure Conf
        <*> transscript
        <*> codeDim
        <*> dpi
        <*> pixelSize
        <*> imageFormat

    transscript = optional $ strOption $ mconcat
        [ long "transscript"
        , short 't'
        , metavar "FILE"
        , help "Mapping from media file indices to plaintext. This should be a ';'-separated file, with OID codes in the first column and plain text in the second"
        ]

    dpi = option auto $ mconcat
        [ long "dpi"
        , metavar "DPI"
        , value 1200
        , showDefault
        , help "Use this resolution in dpi when creating OID codes"
        ]

    pixelSize = option auto $ mconcat
        [ long "pixel-size"
        , metavar "N"
        , value 2
        , showDefault
        , help "Use this many pixels (squared) per dot in when creating OID codes."
        ]

    imageFormat = optional $ option parseImageFormat $ mconcat
        [ long "image-format"
        , short 'f'
        , metavar "Format"
        , showDefaultWith showImageFormat
        , help "image format to write: PNG, PDF, SVG, SVG+PNG (not all commands support all formats)"
        ]

    showImageFormat PNG         = "png"
    showImageFormat PDF         = "pdf"
    showImageFormat (SVG True)  = "svg+png"
    showImageFormat (SVG False) = "png"

    parseImageFormat :: ReadM ImageFormat
    parseImageFormat = eitherReader (go . map toLower)
      where
        go "png"     = return PNG
        go "pdf"     = return PDF
        go "svg"     = return (SVG False)
        go "svg+png" = return (SVG True)
        go f         = Left $ "Unknown image format " ++ f


    codeDim = option parseCodeDim $ mconcat
        [ long "code-dim"
        , metavar "W[xH]"
        , value (30,30)
        , showDefaultWith showCodeDim
        , help "Generate OID codes of this size, in millimeters"
        ]

    showCodeDim (x,y) | x == y    = show x
                      | otherwise = show x ++ "x" ++ show y

    parseCodeDim :: ReadM (Int, Int)
    parseCodeDim = eitherReader go
      where
        go input = case reads input of
            [(x,"")] -> return (x,x)
            [(x,'x':rest)] -> case reads rest of
                [(y,[])] -> return (x,y)
                _        -> Left $ "Cannot parse dimensions " ++ input
            _        -> Left $ "Cannot parse dimensions " ++ input

    cmd :: Parser (Conf -> IO ())
    cmd = asum
        [ hsubparser $ mconcat
          [ commandGroup "GME creation commands:"
          , assembleCmd
          , hidden
          ]
        , hsubparser $ mconcat
          [ commandGroup "OID code creation commands:"
          , oidTableCmd
          , oidCodesCmd
          , oidCodeCmd
          , hidden
          ]
        , hsubparser $ mconcat
          [ commandGroup "GME modification commands:"
          , setLanguageCmd
          , setProductIdCmd
          , hidden
          ]
        , hsubparser $ mconcat
          [ commandGroup "GME analysis commands:"
          , infoCmd
          , exportCmd
          , scriptsCmd
          , scriptCmd
          , gamesCmd
          , lintCmd
          , segmentsCmd
          , segmentCmd
          , explainCmd
          , holesCmd
          , rewriteCmd
          , hidden
          ]
        , hsubparser $ mconcat
          [ commandGroup "GME extraction commands:"
          , mediaCmd
          , binariesCmd
          , hidden
          ]
        , hsubparser $ mconcat
          [ commandGroup "Simulation commands:"
          , playCmd
          ]
        ]

only :: (Eq a, Show a) => [a] -> ReadM a -> ReadM a
only valid r = do
    x <- r
    if x `elem` valid then return x
                      else readerError msg
  where msg = "Sorry, supported values are only: " ++ intercalate ", " (map show valid)

cmdSep :: String -> Mod CommandFields a
cmdSep s = command s $ info empty mempty


-- Common option Parsers

gmeFileParser :: Parser FilePath
gmeFileParser = strArgument $ mconcat
    [ metavar "GME"
    , help "GME file to read"
    ]

yamlFileParser :: Parser FilePath
yamlFileParser = strArgument $ mconcat
    [ metavar "YAML"
    , help "Yaml file to read"
    ]

rawSwitchParser :: Parser Bool
rawSwitchParser = switch $ mconcat
    [ long "raw"
    , help "print the scripts in their raw form"
    ]

-- Individual commands

infoCmd :: Mod CommandFields (Conf -> IO ())
infoCmd =
    command "info" $
    info parser $
    progDesc "Print general information about a GME file"
  where
    parser = flip dumpInfo <$> gmeFileParser


mediaCmd :: Mod CommandFields (Conf -> IO ())
mediaCmd =
    command "media" $
    info parser $
    progDesc "dumps all audio samples"
  where
    parser = const <$> (dumpAudioTo <$> mediaDirParser <*> gmeFileParser)

    mediaDirParser :: Parser FilePath
    mediaDirParser = strOption $ mconcat
        [ long "dir"
        , short 'd'
        , metavar "DIR"
        , help "Media output directory"
        , value "media"
        , showDefault
        ]

scriptsCmd :: Mod CommandFields (Conf -> IO ())
scriptsCmd =
    command "scripts" $
    info parser $
    progDesc "prints the decoded scripts for each OID"
  where
    parser = (\r f c -> dumpScripts c r Nothing f)
        <$> rawSwitchParser
        <*> gmeFileParser


scriptCmd :: Mod CommandFields (Conf -> IO ())
scriptCmd =
    command "script" $
    info parser $
    progDesc "prints the decoded scripts for a specific OID"
  where
    parser = (\r f n c -> dumpScripts c r (Just n) f)
        <$> rawSwitchParser
        <*> gmeFileParser
        <*> scriptParser

    scriptParser = argument auto $ mconcat
        [ metavar "OID"
        , help "OID to look up"
        ]

binariesCmd :: Mod CommandFields (Conf -> IO ())
binariesCmd =
    command "binaries" $
    info parser $
    progDesc "dumps all binaries"
  where
    parser = const <$> (dumpBinariesTo <$> binariesDirParser <*> gmeFileParser)

    binariesDirParser :: Parser FilePath
    binariesDirParser = strOption $ mconcat
        [ long "dir"
        , short 'd'
        , metavar "DIR"
        , help "Binaries output directory"
        , value defaultBinariesPath
        , showDefault
        ]

gamesCmd :: Mod CommandFields (Conf -> IO ())
gamesCmd =
    command "games" $
    info parser $
    progDesc "prints the decoded games"
  where
    parser = flip dumpGames <$> gmeFileParser

lintCmd :: Mod CommandFields (Conf -> IO ())
lintCmd =
    command "lint" $
    info parser $
    progDesc "checks for errors in the file or in this program"
  where
    parser = const <$> (lint <$> gmeFileParser)

segmentsCmd :: Mod CommandFields (Conf -> IO ())
segmentsCmd =
    command "segments" $
    info parser $
    progDesc "lists all known parts of the file, with description."
  where
    parser = const <$> (segments <$> gmeFileParser)


segmentCmd :: Mod CommandFields (Conf -> IO ())
segmentCmd =
    command "segment" $
    info parser $
    progDesc "prints the segment that contains a specific offset"
  where
    parser = (\f n c -> findPosition n f)
        <$> gmeFileParser
        <*> offsetParser

    offsetParser = argument hexReadM $ mconcat
        [ metavar "POS"
        , help "offset into the file to look up, in bytes"
        ]

    hexReadM :: ReadM Integer
    hexReadM = eitherReader go
      where go n | Just int <- readMaybe n = return int
                 | [(int,[])] <- readHex n = return int
                 | otherwise               = Left $ "Cannot parse offset " ++ n

holesCmd :: Mod CommandFields (Conf -> IO ())
holesCmd =
    command "holes" $
    info parser $
    progDesc "lists all unknown parts of the file."
  where
    parser = const <$> (unknown_segments <$> gmeFileParser)

explainCmd :: Mod CommandFields (Conf -> IO ())
explainCmd =
    command "explain" $
    info parser $
    progDesc "print a hexdump of a GME file with descriptions"
  where
    parser = const <$> (explain <$> gmeFileParser <*> don'tSkipParser)

    don'tSkipParser :: Parser Bool
    don'tSkipParser = switch $ mconcat
        [ long "dont-skip"
        , help "don't omit long segments "
        ]

playCmd :: Mod CommandFields (Conf -> IO ())
playCmd =
    command "play" $
    info parser $
    progDesc "interactively play a GME or YAML file"
  where
    parser = flip play <$> gmeOrYamlFileParser

    gmeOrYamlFileParser :: Parser FilePath
    gmeOrYamlFileParser = strArgument $ mconcat
        [ metavar "FILE"
        , help "GME or YAML file to read"
        ]


rewriteCmd :: Mod CommandFields (Conf -> IO ())
rewriteCmd =
    command "rewrite" $
    info parser $
    progDesc "parses the file and writes it again (for debugging)"
  where
    parser = const <$> (rewrite <$> gmeFileParser <*> outFileParser)

    outFileParser :: Parser FilePath
    outFileParser = strArgument $ mconcat
        [ metavar "OUT"
        , help "GME file to write"
        ]

twoFiles :: String -> (FilePath -> FilePath -> a) -> (FilePath -> Maybe FilePath -> a)
twoFiles suffix go inFile (Just outFile) = go inFile outFile
twoFiles suffix go inFile Nothing = go inFile outFile
  where outFile = dropExtension inFile <.> suffix


exportCmd :: Mod CommandFields (Conf -> IO ())
exportCmd =
    command "export" $
    info parser $
    progDesc "dumps the file in the human-readable yaml format"
  where
    parser = const <$> (twoFiles "yaml" export <$> gmeFileParser <*> outFileParser)

    outFileParser :: Parser (Maybe FilePath)
    outFileParser = optional $ strArgument $ mconcat
        [ metavar "OUT"
        , help "YAML file to write"
        ]

assembleCmd :: Mod CommandFields (Conf -> IO ())
assembleCmd =
    command "assemble" $
    info parser $
    progDesc "creates a gme file from the given source"
  where
    parser = (\c a b _conf -> twoFiles "gme" (assemble c) a b)
      <$> noDate <*> yamlFileParser <*> outFileParser

    noDate :: Parser Bool
    noDate = switch $ mconcat
        [ long "no-date"
        , help "do not include today’s date in GME (useful for testing)"
        ]

    outFileParser :: Parser (Maybe FilePath)
    outFileParser = optional $ strArgument $ mconcat
        [ metavar "OUT"
        , help "GME file to write"
        ]

oidTableCmd :: Mod CommandFields (Conf -> IO ())
oidTableCmd =
    command "oid-table" $
    info parser $
    progDesc "creates a PDF or SVG file with all codes in the yaml file"
  where
    parser = (\a b conf ->
        let suffix = suffixOf (fromMaybe PDF (cImageFormat conf)) in
        twoFiles suffix (genOidTable conf) a b
        ) <$> yamlFileParser <*> outFileParser


    outFileParser :: Parser (Maybe FilePath)
    outFileParser = optional $ strArgument $ mconcat
        [ metavar "OUT"
        , help "file to write (default: PDF)"
        ]

oidCodesCmd :: Mod CommandFields (Conf -> IO ())
oidCodesCmd =
    command "oid-codes" $
    info parser $
    progDesc "creates files for every OID in the yaml file (default: PNG)." <>
    footerDoc foot
  where
    foot = unChunk $ vsepChunks
        [ paragraph "Uses oid-<code>.<format> as the file name."
        , paragraph "Use the global options to configure size, file format"
        , paragraph "resolution and blackness of the code (see ./tttool --help)."
        , paragraph $ "Note that it used to work to call \"tttool oid-code foo.yaml\". " ++
                      "Please use \"tttool oid-codes\" for that now."
        ]
    parser = flip writeImagesForFile <$> yamlFileParser

oidCodeCmd :: Mod CommandFields (Conf -> IO ())
oidCodeCmd =
    command "oid-code" $
    info parser $
    progDesc "creates PNG files for each given code(s)" <>
    footerDoc foot
  where
    foot = unChunk $ vsepChunks
        [ paragraph "Uses oid-<code>.png as the file name."
        , paragraph "Use the global options to configure size, resolution and blackness of the code (see ./tttool --help)."
        , paragraph $ "Note that it used to work to call \"tttool oid-code foo.yaml\". " ++
                      "Please use \"tttool oid-codes\" for that now."
        ]

    parser =(\raw range c -> writeImagesForCodes raw c range) <$> rawCodeSwitchParser <*> codeRangeParser

    codeRangeParser :: Parser [Word16]
    codeRangeParser = argument (eitherReader parseRange) $ mconcat
        [ metavar "RANGE"
        , help "OID range, for example e.g. 1,3,1000-1085."
        ]

    rawCodeSwitchParser :: Parser Bool
    rawCodeSwitchParser = switch $ mconcat
        [ long "raw"
        , help "take the given codes as \"raw codes\" (rarely needed)"
        ]

setLanguageCmd :: Mod CommandFields (Conf -> IO ())
setLanguageCmd =
    command "set-language" $
    info parser $
    progDesc "sets the language field of an GME file" <>
    footerDoc foot
  where
    foot = unChunk $ vsepChunks
        [ paragraph $ unwords
          [ "If the language of a GME file is not empty and does not"
          , "match the pen\'s language, the pen will refuse to play it."
          , "So for example you cannot play a French GME file using a pen"
          , "set to German. This command allows you to adjust a GME"
          , "file\'s language, and either remove it completely (with --empty)"
          , "or to match the pen\'s language, so you can play the GME file."
          ]
        , paragraph $ unwords
          [ "WARNING: This modifies the given GME file in place."
          ]
        ]

    parser = (\l f c -> setLanguage l f) <$> languageParser <*> gmeFileParser

    gmeFileParser :: Parser FilePath
    gmeFileParser = strArgument $ mconcat
        [ metavar "GME"
        , help "GME file to modify"
        ]

    languageParser :: Parser String
    languageParser =
      flag' "" (long "empty") <|>
      strArgument (mconcat
        [ metavar "LANG"
        , help "Language (e.g. GERMAN, ENGLISH, FRENCH, DUTCH…)"
        ])

setProductIdCmd :: Mod CommandFields (Conf -> IO ())
setProductIdCmd =
    command "set-product-id" $
    info parser $
    progDesc "changes the product id an GME file" <>
    footerDoc foot
  where
    foot = unChunk $ vsepChunks
        [ paragraph $ unwords
          [ "It may be useful change the product id of a GME file without touching"
          , "the rest of the file."
          ]
        , paragraph $ unwords
          [ "WARNING: This modifies the given GME file in place."
          ]
        ]

    parser = (\l f c -> setProductId l f) <$> productIdParser <*> gmeFileParser

    gmeFileParser :: Parser FilePath
    gmeFileParser = strArgument $ mconcat
        [ metavar "GME"
        , help "GME file to modify"
        ]

    productIdParser :: Parser ProductID
    productIdParser =
      argument auto (mconcat
        [ metavar "PRODUCT-ID"
        , help "Product id (typically < 1000)"
        ])

main :: IO ()
main = do
    act <- customExecParser (prefs showHelpOnError) optionParser
    act
