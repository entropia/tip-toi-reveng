{-# LANGUAGE TypeSynonymInstances #-}
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
groupRuns :: (Eq a, Enum a) => (a -> Maybe b) -> [a] -> [Either b [a]]
groupRuns l = go
  where
    go []  = []
    go [x] = case l x of Nothing -> [Right [x]]
                         Just s  -> [Left s]
    go (x:xs) = case l x of
        Just l -> Left l : go xs
        Nothing -> case go xs of
            Right (y:ys):r' | succ x == y -> Right (x:y:ys) : r'
            r                             -> Right [x] : r

ppPlayList :: Transscript -> PlayList -> String
ppPlayList t xs = "[" ++ commas (map go (groupRuns (flip M.lookup t) xs)) ++ "]"
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
ppCommand True t xs (Play n)     | not (validIndex xs (fromIntegral n)) = ""
ppCommand True t xs (Random a b) | any (not . validIndex xs . fromIntegral) [b..a] = ""

ppCommand _ t xs (Play n)        = printf "P(%s)" (ppPlayIndex t xs (fromIntegral n))
ppCommand _ t xs (Random a b)    = printf "P(%s)" $ commas $ map (ppPlayIndex t xs . fromIntegral ) [b..a]
ppCommand _ t xs (Cancel)        = printf "C"
ppCommand _ t xs (Jump v)        = printf "J(%s)" (ppTVal v)
ppCommand _ t xs (NamedJump v)   = printf "J(%s)" v
ppCommand _ t xs (Game b)        = printf "G(%d)" b
ppCommand _ t xs (ArithOp o r n) = ppReg r ++ ppArithOp o ++ ppTVal n
ppCommand _ t xs (Neg r)       = printf "Neg(%s)" (ppReg r)
ppCommand _ t xs (Unknown b r n) = printf "?(%s,%s) (%s)" (ppReg r) (ppTVal n) (prettyHex b)

validIndex :: PlayList -> Int -> Bool
validIndex xs n = n >= 0 && n < length xs

ppPlayIndex :: Transscript -> PlayList -> Int -> String
ppPlayIndex t xs n | validIndex xs n = transcribe t (xs !! n)
                   | otherwise       = "invalid_index_" ++ show n

spaces = intercalate " "
commas = intercalate ","
quote s = printf "'%s'" s

ppGame :: Transscript -> Game -> String
ppGame t (Game6 u1 u2 plls sg1s sg2s u3 pll2s pl) =
    printf (unlines ["  type: 6", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames1: (%d)", "%s",
                     "  subgames2: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  playlist: %s"])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sg1s)   (concatMap (ppSubGame t) sg1s)
    (length sg2s)   (concatMap (ppSubGame t) sg2s)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (show pl)
ppGame t (Game7 u1 c u2 plls sgs u3 pll2s pll) =
    printf (unlines ["  type: 7", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  playlistlist: %s"])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (ppPlayListList t pll)
ppGame t (Game8 u1 c u2 plls sgs u3 pll2s oidl gidl pll1 pll2) =
    printf (unlines ["  type: 8", "  u1:   %d", "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s",
                     "  oids: %s",
                     "  gids: %s",
                     "  playlistlist: %s",
                     "  playlistlist: %s"
                     ])
    u1 (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
    (ppOidList oidl) (show gidl)
    (ppPlayListList t pll1) (ppPlayListList t pll2)
ppGame t (UnknownGame typ u1 c u2 plls sgs u3 pll2s) =
    printf (unlines ["  type: %d",
                     "  u1:   %d",
                     "  c:    %d",
                     "  u2:   %s",
                     "  playlistlists: (%d)", "%s",
                     "  subgames: (%d)", "%s",
                     "  u3: %s",
                     "  playlistlists: (%d)","%s"])
    typ u1 c (prettyHex u2)
    (length plls)   (indent 4 (map (ppPlayListList t) plls))
    (length sgs)    (concatMap (ppSubGame t) sgs)
    (prettyHex u3)
    (length pll2s)  (indent 4 (map (ppPlayListList t) pll2s))
ppGame t Game253 =
    printf (unlines ["  type: 253"
                     ])
ppGame t _ = "TODO"

ppSubGame :: Transscript -> SubGame -> String
ppSubGame t (SubGame u oids1 oids2 oids3 plls) = printf (unlines
    [ "    Subgame:"
    , "      u: %s"
    , "      oids1: %s"
    , "      oids2: %s"
    , "      oids3: %s"
    , "      playlistlists: (%d)" , "%s"
    ])
    (prettyHex u)
    (ppOidList oids1) (ppOidList oids2) (ppOidList oids3)
    (length plls)  (indent 8 (map (ppPlayListList t) plls))

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
