{-# LANGUAGE TypeSynonymInstances, RecordWildCards #-}
module PrettyPrint where

import qualified Data.ByteString.Lazy as B
import Text.Printf
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Types

-- Pretty printing

lineHex bytes l = prettyHex $ extract (lineOffset l) (lineLength l) bytes

extract :: Offset -> Word32 -> B.ByteString ->  B.ByteString
extract off len = B.take  (fromIntegral len) . B.drop (fromIntegral off)

lineOffset (Line o _ _ _) = o

lineLength :: Line r -> Word32
lineLength (Line _ conds cmds audio) = fromIntegral $
    2 + 8 * length conds + 2 + 7 * length cmds + 2 + 2 * length audio

ppLine :: Transscript -> Line ResReg -> String
ppLine t (Line _ cs as xs) = spaces $
    map ppConditional cs ++ map (ppCommand False t xs) as

-- Varaint that does not generate invalid play commands
exportLine :: Line ResReg -> String
exportLine (Line _ cs as xs) = spaces $
    map ppConditional cs ++ map (ppCommand True M.empty xs) as

-- Group consecutive runs of numbers, if they do not have a description
groupRuns :: (Eq a, Bounded a, Enum a) => (a -> Maybe b) -> [a] -> [Either b [a]]
groupRuns l = go
  where
    go []  = []
    go [x] = case l x of Nothing -> [Right [x]]
                         Just s  -> [Left s]
    go (x:xs) = case l x of
        Just l -> Left l : go xs
        Nothing -> case go xs of
            Right (y:ys):r'
              | x /= maxBound
              , succ x == y    -> Right (x:y:ys) : r'
            r                  -> Right [x] : r

ppPlayList :: Transscript -> PlayList -> String
ppPlayList t xs = "[" ++ commas (map go (groupRuns (`M.lookup` t) xs)) ++ "]"
  where go (Left s) = quote s
        go (Right [])  = error "Empty list in groupRuns result"
        go (Right l)   | length l > 3 = show (head l) ++ ".." ++ show (last l)
                       | otherwise    = commas (map show l)

ppOidList :: [OID] -> String
ppOidList xs = "[" ++ commas (map go (groupRuns (const Nothing) xs)) ++ "]"
  where go (Left s) = s
        go (Right [])  = error "Empty list in groupRuns result"
        go (Right l)   | length l > 3 = show (head l) ++ ".." ++ show (last l)
                       | otherwise    = commas (map show l)

ppPlayListList :: Transscript -> PlayListList -> String
ppPlayListList t xs = "[" ++ commas (map (ppPlayList t) xs) ++ "]"

ppConditional :: Conditional ResReg -> String
ppConditional (Cond v1 o v2) = printf "%s%s%s?" (ppTVal v1) (ppCondOp o) (ppTVal v2)

ppCondOp :: CondOp -> String
ppCondOp Eq              = "=="
ppCondOp NEq             = "!="
ppCondOp Lt              = "< "
ppCondOp Gt              = "> "
ppCondOp GEq             = ">="
ppCondOp LEq             = "<="
ppCondOp (Unknowncond b) = printf "?%s?" (prettyHex b)

ppTVal :: Reg r => TVal r -> String
ppTVal (Reg r)   =  ppReg r
ppTVal (Const n) =  show n


ppResReg :: ResReg -> String
ppResReg n = "$" ++ show n

ppRegister :: Register -> String
ppRegister (RegPos n) = "$" ++ show n
ppRegister (RegName n) = "$" ++ n

class Reg a where
  ppReg :: a -> String

instance Reg ResReg   where ppReg = ppResReg
instance Reg Register where ppReg = ppRegister

ppArithOp :: ArithOp -> String
ppArithOp Inc  = "+="
ppArithOp Dec  = "-="
ppArithOp Mult = "*="
ppArithOp Div  = "/="
ppArithOp Mod  = "%="
ppArithOp And  = "&="
ppArithOp Or   = "|="
ppArithOp XOr  = "^="
ppArithOp Set  = ":="

ppCommand :: Reg r => Bool -> Transscript -> PlayList -> Command r -> String
ppCommand True t xs p
    | any (not . validIndex xs) (indices p) = ""

ppCommand _ t xs (Play n)        = printf "P(%s)" $ ppPlayIndex t xs (fromIntegral n)
ppCommand _ t xs (Random a b)    = printf "P(%s)" $ ppPlayRange t xs [b..a]
ppCommand _ t xs (PlayAll a b)   = printf "PA(%s)" $ ppPlayRange t xs [b..a]
ppCommand _ t xs (PlayAllVariant (Const 0)) = printf "PA*(%s)"    (ppPlayAll t xs)
ppCommand _ t xs (PlayAllVariant v)         = printf "PA*(%s)(%s)" (ppPlayAll t xs) (ppTVal v)
ppCommand _ t xs (RandomVariant (Const 0))  = printf "P*(%s)"     (ppPlayAll t xs)
ppCommand _ t xs (RandomVariant v)          = printf "P*(%s)(%s)"  (ppPlayAll t xs) (ppTVal v)
ppCommand _ t xs Cancel          = printf "C"
ppCommand _ t xs (Jump v)        = printf "J(%s)" (ppTVal v)
ppCommand _ t xs (Timer r v)     = printf "T(%s,%s)" (ppReg r) (ppTVal v)
ppCommand _ t xs (NamedJump v)   = printf "J(%s)" v
ppCommand _ t xs (Game b)        = printf "G(%d)" b
ppCommand _ t xs (ArithOp o r n) = ppReg r ++ ppArithOp o ++ ppTVal n
ppCommand _ t xs (Neg r)       = printf "Neg(%s)" (ppReg r)
ppCommand _ t xs (Unknown b r n) = printf "?(%s,%s) (%s)" (ppReg r) (ppTVal n) (prettyHex b)

indices :: Command r -> [Int]
indices (Play n)      = [fromIntegral n]
indices (Random a b)  = map fromIntegral [b..a]
indices (PlayAll a b) = map fromIntegral [b..a]
indices _ = []

validIndex :: PlayList -> Int -> Bool
validIndex xs n = n >= 0 && n < length xs

ppPlayIndex :: Transscript -> PlayList -> Int -> String
ppPlayIndex t xs n | validIndex xs n = transcribe t (xs !! n)
                   | otherwise       = "invalid_index_" ++ show n

ppPlayRange :: Transscript -> PlayList -> [Word8] -> String
ppPlayRange t xs = commas . map (ppPlayIndex t xs . fromIntegral)

ppPlayAll :: Transscript -> PlayList -> String
ppPlayAll t = commas . map (transcribe t)

spaces = intercalate " "
commas = intercalate ","
quote s = printf "'%s'" s

ppCommonGame :: Transscript -> Game -> String
ppCommonGame t g =
    printf (unlines ["  type: %d",
                     "  rounds: %d",
                     "  unknown (c): %d",
                     "  early rounds: %d",
                     "  repeat last media OID: %d",
                     "  unknown (x): %d",
                     "  unknown (w): %d",
                     "  unknown (v): %d",
                     "  start play list:               %s",
                     "  round end play list:           %s",
                     "  finish play list:              %s",
                     "  round start play list:         %s",
                     "  later round start play list:   %s",
                     "  subgames: (%d)", "%s",
                     "  target scores: (%d) %s",
                     "  finish play lists: (%d)", "%s"
                     ])
    (gameType g)
    (gRounds g)
    (gUnknownC g)
    (gEarlyRounds g)
    (gRepeatLastMedia g)
    (gUnknownX g) (gUnknownW g) (gUnknownV g)
    (ppPlayListList t (gStartPlayList g))
    (ppPlayListList t (gRoundEndPlayList g))
    (ppPlayListList t (gFinishPlayList g))
    (ppPlayListList t (gRoundStartPlayList g))
    (ppPlayListList t (gLaterRoundStartPlayList g))
    (length (gSubgames g))       (ppSubGames t (gSubgames g))
    (length (gTargetScores g))   (show (gTargetScores g))
    (length (gFinishPlayLists g))(indent 4 (map (ppPlayListList t) (gFinishPlayLists g)))

ppGame :: Transscript -> Game -> String
ppGame t g@(CommonGame {..}) =
    ppCommonGame t g

ppGame t (Game6 {..}) =
    printf (unlines ["  type: 6",
                     "  rounds: %d",
                     "  bonus rounds: %d",
                     "  rounds target: %d",
                     "  unknown (i): %d",
                     "  early rounds: %d",
                     "  unknown (q): %d",
                     "  repeat last media OID: %d",
                     "  unknown (x): %d",
                     "  unknown (w): %d",
                     "  unknown (v): %d",
                     "  start play list:               %s",
                     "  round end play list:           %s",
                     "  finish play list:              %s",
                     "  round start play list:         %s",
                     "  later round start play list:   %s",
                     "  round start play list 2:       %s",
                     "  later round start play list 2: %s",
                     "  bonus subgame count: %d",
                     "  subgames: (%d)", "%s",
                     "  target scores: (%d) %s",
                     "  bonus target scores: (%d) %s",
                     "  finish play lists: (%d)", "%s",
                     "  bonus finish play lists: (%d)", "%s",
                     "  bonus subgame ids: %s"
                     ])
    gRounds
    gBonusRounds
    gBonusTarget
    gUnknownI
    gEarlyRounds
    gUnknownQ
    gRepeatLastMedia
    gUnknownX gUnknownW gUnknownV
    (ppPlayListList t gStartPlayList)
    (ppPlayListList t gRoundEndPlayList)
    (ppPlayListList t gFinishPlayList)
    (ppPlayListList t gRoundStartPlayList)
    (ppPlayListList t gLaterRoundStartPlayList)
    (ppPlayListList t gRoundStartPlayList2)
    (ppPlayListList t gLaterRoundStartPlayList2)
    gBonusSubgameCount
    (length gSubgames)           (ppSubGames t gSubgames)
    (length gTargetScores)       (show gTargetScores)
    (length gBonusTargetScores)  (show gBonusTargetScores)
    (length gFinishPlayLists)    (indent 4 (map (ppPlayListList t) gFinishPlayLists))
    (length gBonusFinishPlayLists)    (indent 4 (map (ppPlayListList t) gBonusFinishPlayLists))
    (show gBonusSubgameIds)

ppGame t g@(Game7 {..}) = (ppCommonGame t g ++) $
    printf (unlines [ "  subgame groups: %s"
                     ])
        (show gSubgameGroups)

ppGame t g@(Game8 {..}) = (ppCommonGame t g ++) $
    printf (unlines ["  game select OIDs:     %s",
                     "  game select games:    %s",
                     "  game select errors 1: %s",
                     "  game select errors 2: %s"
                     ])
        (show gGameSelectOIDs)
        (show gGameSelect)
        (ppPlayListList t gGameSelectErrors1)
        (ppPlayListList t gGameSelectErrors2)

ppGame t g@(Game9 {..}) = (ppCommonGame t g ++) $
    printf (unlines ["  extra play lists (%d):","%s"
                     ])
        (length gExtraPlayLists)  (indent 4 (map (ppPlayListList t) gExtraPlayLists))

ppGame t g@(Game10 {..}) = (ppCommonGame t g ++) $
    printf (unlines ["  extra play lists (%d):","%s"
                     ])
        (length gExtraPlayLists)  (indent 4 (map (ppPlayListList t) gExtraPlayLists))

ppGame t g@(Game16 {..}) = (ppCommonGame t g ++) $
    printf (unlines ["  extra OIDs (%d): %s",
                     "  extra play lists (%d):","%s"
                     ])
        (length gExtraOIDs)       (show gExtraOIDs)
        (length gExtraPlayLists)  (indent 4 (map (ppPlayListList t) gExtraPlayLists))

ppGame t Game253 =
    printf (unlines ["  type: 253"
                     ])

ppSubGames :: Transscript -> [SubGame] -> String
ppSubGames t = concatMap (uncurry (ppSubGame t)) . zip [0..]

ppSubGame :: Transscript -> Int -> SubGame -> String
ppSubGame t n (SubGame u oids1 oids2 oids3 plls) = printf (unlines
    [ "    Subgame %d:"
    , "      u: %s"
    , "      oids1: %s"
    , "      oids2: %s"
    , "      oids3: %s"
    , "      playlist: (%d)" , "%s"
    ])
    n
    (prettyHex u)
    (ppOidList oids1) (ppOidList oids2) (ppOidList oids3)
    (length plls)  (indent 8 (map (ppPlayListList t) plls))

indent :: Int -> [String] -> String
indent n = intercalate "\n" . map (replicate n ' ' ++)

checkLine :: Int -> Line ResReg -> [String]
checkLine n_audio l@(Line _ _ _ xs)
    | any (>= fromIntegral n_audio) xs
    = return $ "Invalid audio index in line " ++ ppLine M.empty l
checkLine n_audio _ = []


prettyHex :: B.ByteString -> String
prettyHex = intercalate " " . map (printf "%02X") . B.unpack

transcribe :: Transscript -> Word16 -> String
transcribe t idx = fromMaybe (show idx) (M.lookup idx t)

ppDesc :: [String] -> String
ppDesc = intercalate "/"
