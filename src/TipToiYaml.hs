{-# LANGUAGE RecordWildCards, DeriveGeneric #-}

module TipToiYaml
    ( tt2ttYaml, ttYaml2tt
    , readTipToiYaml, writeTipToiYaml,writeTipToiCodeYaml
    , ttyProduct_Id
    )
where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Char8 as SBC
import System.Exit
import System.FilePath
import Text.Printf
import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import Control.Monad
import System.Directory
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Writer.Strict
import Data.Time
import System.Locale
import Data.Yaml hiding ((.=), Parser)
import Data.Aeson.Types hiding ((.=), Parser)
import Text.Parsec hiding (Line, lookAhead, spaces)
import Text.Parsec.String
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import GHC.Generics
import qualified Data.Foldable as F
import Control.Arrow
import Control.Applicative ((<*>), (<*))

import Types
import Constants
import PrettyPrint
import OneLineParser
import Utils

type CodeMap = M.Map String Word16

data TipToiYAML = TipToiYAML
    { ttyScripts :: M.Map String [String]
    , ttyComment :: Maybe String
    , ttyMedia_Path :: Maybe String
    , ttyInit :: Maybe String
    , ttyWelcome :: Maybe String
    , ttyProduct_Id :: Word32
    , ttyScriptCodes :: Maybe CodeMap
    }
    deriving Generic

data TipToiCodesYAML = TipToiCodesYAML
    { ttcScriptCodes :: CodeMap
    }
    deriving Generic

options = defaultOptions { fieldLabelModifier = map fix . map toLower . drop 3 }
       where fix '_' = '-'
             fix c   = c

instance FromJSON TipToiYAML where
     parseJSON = genericParseJSON $ options
instance ToJSON TipToiYAML where
     toJSON = genericToJSON $ options
instance FromJSON TipToiCodesYAML where
     parseJSON = genericParseJSON $ options
instance ToJSON TipToiCodesYAML where
     toJSON = genericToJSON $ options


tt2ttYaml :: String -> TipToiFile -> TipToiYAML
tt2ttYaml path (TipToiFile {..}) = TipToiYAML
    { ttyProduct_Id = ttProductId
    , ttyInit = Just $ spaces $ [ ppCommand True M.empty [] (ArithOp Set (RegPos r) (Const n))
                                | (r,n) <- zip [0..] ttInitialRegs , n /= 0]
    , ttyWelcome = Just $ commas $ map show $ concat ttWelcome
    , ttyComment = Just $ BC.unpack ttComment
    , ttyScripts = M.fromList
        [ (show oid, map exportLine ls) | (oid, Just ls) <- ttScripts]
    , ttyMedia_Path = Just path
    , ttyScriptCodes = Nothing
    }


mergeOnlyEqual :: String -> Word16 -> Word16 -> Word16
mergeOnlyEqual _ c1 c2 | c1 == c2 = c1
mergeOnlyEqual s c1 c2 = error $
    printf "The .yaml file specifies code %d for script \"%s\",\
           \but the .codes.yamls file specifies %d. Please fix this!" c2 s c1

scriptCodes :: [String] -> CodeMap -> Either String (String -> Word16, CodeMap)
scriptCodes [] codeMap = Right (error "scriptCodes []", codeMap)
scriptCodes codes codeMap
    | null strs = Right (readCode, codeMap)
    | length strs > length availableCodes = Left "Too many codes used"
    | null nums = Right (lookupCode, totalMap)
    | otherwise = Left "Cannot mix numbers and names in scripts."
  where
    (strs, nums) = partitionEithers $ map f codes
    newStrs = filter (`M.notMember` codeMap) strs
    usedCodes = S.fromList $ M.elems codeMap

    availableCodes = filter (`S.notMember` usedCodes) knownCodes

    f s = case readMaybe s of
            Nothing -> Left s 
            Just n -> Right (n::Word16)
    newAssignments = M.fromList (zip newStrs availableCodes)
    totalMap = M.unionWithKey (\_ _ _ -> error "scriptCodes: conflict!") codeMap newAssignments

    readCode s = case readMaybe s of
        Nothing -> error $ printf "Cannot jump to named script \"%s\" in a yaml with numbered scripts." s
        Just c -> c
    lookupCode s = case M.lookup s totalMap of
        Nothing -> error $ printf "Cannot jump to unknown script \"%s\"." s
        Just c -> c

resolveRegs :: (M.Map Register Word16, [(t0, Maybe [Line Register])]) -> (M.Map ResReg Word16, [(t0, Maybe [Line ResReg])])
resolveRegs x = everywhere x
  where
    -- Could use generics somehow
    regs = S.fromList $
        M.keys (fst x) ++ concatMap (maybe [] (concatMap F.toList) . snd) (snd x)
    -- Could use generics somehow
    everywhere = M.mapKeys resolve *** map (second (fmap (map (fmap resolve))))


    regNums = S.fromList [ n | RegPos n <- S.toList regs ]
    regNames = [ n | RegName n <- S.toList regs ]
    mapping = M.fromList $ zip regNames [n | n <- [0..], n `S.notMember` regNums]
    resolve (RegPos n) = n
    resolve (RegName n) = fromMaybe (error "resolveRegs broken") (M.lookup n mapping)

resolveJumps :: (String -> Word16) -> [(a, Maybe [Line b])] -> [(a, Maybe [Line b])]
resolveJumps m = everywhere
    where 
    everywhere = map (second (fmap (map resolveLine)))
    resolveLine (Line o cond cmds acts) = (Line o cond (map resolve cmds) acts)
    resolve (NamedJump n) = Jump (Const (m n))
    resolve c = c


ttYaml2tt :: FilePath -> TipToiYAML -> CodeMap -> IO (TipToiFile, CodeMap)
ttYaml2tt dir (TipToiYAML {..}) extCodeMap = do
    now <- getCurrentTime
    let date = formatTime defaultTimeLocale "%Y%m%d" now

    let codeMap = M.unionWithKey mergeOnlyEqual
                                 extCodeMap
                                 (fromMaybe M.empty ttyScriptCodes)

    (scriptMap, totalMap) <- case scriptCodes (M.keys ttyScripts) codeMap of
        Left e -> fail e
        Right f -> return f

    let m = M.mapKeys scriptMap ttyScripts
        first = fst (M.findMin m)
        last = fst (M.findMax m)

    welcome_names <- parseOneLine parseWelcome "welcome" (fromMaybe "" ttyWelcome)

    (prescripts, filenames) <- liftM unzip $ forM [first .. last] $ \oid -> do
       case M.lookup oid m of
        Nothing -> return (\_ -> (oid, Nothing), [])
        Just raw_lines -> do
            (lines, filenames) <- liftM unzip $ forMn raw_lines $ \i raw_line -> do
                let d = printf "Line %d of OID %d" i oid
                (l,s) <- parseOneLine parseLine d raw_line
                return (\f -> l (map f s), s)
            return (\f -> (oid, Just (map ($ f) lines)), concat filenames)

    let filenames' = S.toList $ S.fromList $ welcome_names ++ concat filenames
    let filename_lookup = (M.fromList (zip filenames' [0..]) M.!)

    let welcome = [map filename_lookup welcome_names]

    preInitRegs <- M.fromList <$> parseOneLine parseInitRegs "init" (fromMaybe "" ttyInit)

    -- resolve registers
    let (initRegs, scripts) = resolveRegs (preInitRegs, map ($ filename_lookup) prescripts)

    -- resolve named jumps
    let scripts' = resolveJumps scriptMap scripts

    let maxReg = maximum $ 0:
            [ r
            | (_, Just ls) <- scripts'
            , Line _ cs as _ <- ls
            , r <- concatMap F.toList cs ++ concatMap F.toList as ]



    files <- forM filenames' $ \fn -> do
        let paths = [ combine dir relpath
                    | ext <- map snd fileMagics
                    , let pat = fromMaybe "%s" ttyMedia_Path
                    , let relpath = printf pat fn <.> ext
                    ]
        ex <- filterM doesFileExist paths
        case ex of
            [] -> do
                putStrLn "Could not find any of these files:"
                mapM_ putStrLn paths
                exitFailure
            [f] -> B.readFile f
            _  -> do
                putStrLn "Multiple matching files found:"
                mapM_ putStrLn ex
                exitFailure

    return $ (TipToiFile
        { ttProductId = ttyProduct_Id
        , ttRawXor = 0x00000039 -- from Bauernhof
        , ttComment = BC.pack (fromMaybe "created with tip-toi-reveng" ttyComment)
        , ttDate = BC.pack date
        , ttWelcome = welcome
        , ttInitialRegs = [fromMaybe 0 (M.lookup r initRegs) | r <- [0..maxReg]]
        , ttScripts = scripts'
        , ttGames = []
        , ttAudioFiles = files
        , ttAudioXor = 0xAD
        , ttAudioFilesDoubles = False
        , ttChecksum = 0x00
        , ttChecksumCalc = 0x00
        }, totalMap)


lexer       = P.makeTokenParser $
    emptyDef
        { P.reservedOpNames = words ":= == /= < >="
        , P.opLetter       = oneOf ":!#%&*+./<=>?@\\^|-~" -- Removed $, used for registers
        }

parseLine :: Parser ([Word16] -> Line Register, [String])
parseLine = do
    conds <- many (P.try parseCond)
    (acts, filenames) <- parseCommands 0
    eof
    return (Line 0 conds acts, filenames)

descP d p = p <?> d

parseCond :: Parser (Conditional Register)
parseCond = descP "Conditional" $ do
    v1 <- parseTVal
    op <- parseCondOp
    v2 <- parseTVal
    P.lexeme lexer (char '?')
    return (Cond v1 op v2)

parseWord16 :: Parser Word16
parseWord16 = fromIntegral <$> P.natural lexer

parseReg :: Parser Register
parseReg = P.lexeme lexer $ char '$' >> (RegPos <$> parseWord16 <|> RegName <$> many1 (alphaNum <|> char '_'))

parseTVal :: Parser (TVal Register)
parseTVal = (Reg <$> parseReg <|> Const <$> parseWord16) <?> "Value"

parseCondOp :: Parser CondOp
parseCondOp = choice
    [ P.reservedOp lexer "==" >> return Eq
    , P.reservedOp lexer "<"  >> return Lt
    , P.reservedOp lexer ">"  >> return Gt
    , P.reservedOp lexer ">=" >> return GEq
    , P.reservedOp lexer "<=" >> return LEq
    , P.reservedOp lexer "/=" >> return NEq
    , P.reservedOp lexer "!=" >> return NEq
    ]

parseInitRegs :: Parser [(Register, Word16)]
parseInitRegs = many $ do
    r <- parseReg
    P.reservedOp lexer ":="
    v <- parseWord16
    return (r,v)

parseWelcome :: Parser [String]
parseWelcome = P.commaSep lexer $ parseAudioRef

parseAudioRef :: Parser String
parseAudioRef = P.lexeme lexer $ many1 (alphaNum <|> char '_')

parseScriptRef :: Parser String
parseScriptRef = P.lexeme lexer $ many1 (alphaNum <|> char '_')

parsePrettyHex :: Parser B.ByteString
parsePrettyHex = B.pack <$> many1 (P.lexeme lexer nibble)
  where
    nibble = fromIntegral <$> number 16 hexDigit
    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }


parseCommands :: Int -> Parser ([Command Register], [String])
parseCommands i =
    choice
    [ eof >> return ([],[])
    , descP "Register action" $
      do r <- parseReg
         op <- choice [ P.reservedOp lexer ":=" >> return Set
                      , P.reservedOp lexer "+=" >> return Inc
                      , P.reservedOp lexer "-=" >> return Dec
                      , P.reservedOp lexer "%=" >> return Mod
                      , P.reservedOp lexer "/=" >> return Div
                      , P.reservedOp lexer "*=" >> return Mult
                      , P.reservedOp lexer "&=" >> return And
                      , P.reservedOp lexer "|=" >> return Or
                      , P.reservedOp lexer "^=" >> return XOr
                      ]
         v <- parseTVal
         (cmds, filenames) <- parseCommands i
         return (ArithOp op r v : cmds, filenames)

    , descP "Negation" $
      do P.lexeme lexer $ string "Neg"
         r <- P.parens lexer $ parseReg
         (cmds, filenames) <- parseCommands i
         return (Neg r : cmds, filenames)

    , descP "Unknown action" $
      do P.lexeme lexer $ char '?'
         (r,v) <- P.parens lexer $
            (,) <$> parseReg <* P.comma lexer <*> parseTVal
         h <- P.parens lexer parsePrettyHex
         (cmds, filenames) <- parseCommands i
         return (Unknown h r v : cmds, filenames)

    , descP "Play action" $
      do char 'P'
         fns <- P.parens lexer $ P.commaSep1 lexer parseAudioRef
         let n = length fns
         (cmds, filenames) <- parseCommands (i+n)
         let c = case fns of
                [fn] -> Play (fromIntegral i)
                _    -> Random (fromIntegral (i + n - 1)) (fromIntegral i)
         return (c : cmds, fns ++ filenames)
    , descP "Cancel" $
      do P.lexeme lexer $ char 'C'
         (cmds, filenames) <- parseCommands i
         return (Cancel : cmds, filenames)
    , descP "Jump action" $
      do P.lexeme lexer $ char 'J'
         cmd <- P.parens lexer $ choice
            [ Jump <$> parseTVal
            , NamedJump <$> parseScriptRef
            ]
         (cmds, filenames) <- parseCommands i
         return (cmd : cmds, filenames)
    , descP "Start Game" $
      do P.lexeme lexer $ char 'G'
         n <- P.parens lexer $ parseWord16
         (cmds, filenames) <- parseCommands i
         return (Game n : cmds, filenames)
    ]




encodeFileCommented :: ToJSON a => FilePath -> String -> a -> IO ()
encodeFileCommented fn c v = do
    SBC.writeFile fn $ SBC.pack c <> encode v

readTipToiYaml :: FilePath -> IO (TipToiYAML, CodeMap)
readTipToiYaml inf = do
    etty <- decodeFileEither inf
    tty <- case etty of
        Left e -> print e >> exitFailure
        Right tty -> return tty

    ex <- doesFileExist infCodes
    codeMap <-
        if ex
        then do
            ettcy <- decodeFileEither infCodes
            case ettcy of
                Left e -> print e >> exitFailure
                Right ttcy -> return (ttcScriptCodes ttcy)
        else return M.empty
    return (tty, codeMap)
  where
    infCodes = codeFileName inf

writeTipToiYaml :: FilePath -> TipToiYAML -> IO ()
writeTipToiYaml out tty = encodeFile out tty

writeTipToiCodeYaml :: FilePath -> TipToiYAML -> CodeMap -> CodeMap -> IO ()
writeTipToiCodeYaml inf tty oldMap totalMap = do
    let newCodeMap = totalMap M.\\ fromMaybe M.empty (ttyScriptCodes tty)

    ex <- doesFileExist infCodes

    if M.null newCodeMap
    then when ex $ removeFile infCodes
    else when (newCodeMap /= oldMap) $ encodeFileCommented infCodes codesComment (TipToiCodesYAML { ttcScriptCodes = newCodeMap })
  where
    infCodes = codeFileName inf

codesComment :: String
codesComment = unlines $ map ("# " ++)
    [ "This file contains a mapping from script names to oid codes."
    , "This way the existing scripts are always assigned to the the"
    , "same codes, even if you add further scripts."
    , ""
    , "You can copy the contents of this file into the main .yaml file,"
    , "if you want to have both together."
    , ""
    , "If you delete this file, the next run of \"ttool assemble\" might"
    , "use different codes for your scripts, and you might have to re-"
    , "create the images for your product."
    ]

codeFileName :: FilePath -> FilePath
codeFileName fn = base <.> "codes" <.> ext
  where
    base = dropExtension fn
    ext  = takeExtension fn
