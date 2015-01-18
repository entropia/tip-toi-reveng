module GMERun (playTipToi) where

import Text.Printf
import Data.Bits
import Data.List
import qualified Data.Map as M

import Types
import PrettyPrint
import Utils

type PlayState r = M.Map r Word16

formatState :: PlayState ResReg -> String
formatState s = spaces $
    map (\(k,v) -> printf "$%d=%d" k v) $
    filter (\(k,v) -> k == 0 || v /= 0) $
    M.toAscList s

playTipToi :: Transscript -> TipToiFile -> IO ()
playTipToi t tt = do
    let initialState = M.fromList $ zip [0..] (ttInitialRegs tt)
    printf "Initial state (not showing zero registers): %s\n" (formatState initialState)
    forEachNumber initialState $ \i s -> do
        case lookup (fromIntegral i) (ttScripts tt) of
            Nothing -> printf "OID %d not in main table\n" i >> return s
            Just Nothing -> printf "OID %d deactivated\n" i >> return s
            Just (Just lines) -> do
                case find (enabledLine s) lines of
                    Nothing -> printf "None of these lines matched!\n" >> mapM_ (putStrLn . ppLine t) lines >> return s
                    Just l -> do
                        printf "Executing:  %s\n" (ppLine t l)
                        let s' = applyLine l s
                        printf "State now: %s\n" (formatState s')
                        return s'

enabledLine :: Ord r => PlayState r -> Line r -> Bool
enabledLine s (Line _ cond _ _) = all (condTrue s) cond

condTrue :: Ord r => PlayState r -> Conditional r -> Bool
condTrue s (Cond v1 o v2) = value s v1 =?= value s v2
  where
    (=?=) = case o of
        Eq  -> (==)
        NEq -> (/=)
        Lt  -> (<)
        GEq -> (>=)
        _   -> \_ _ -> False

value :: Ord r => PlayState r -> TVal r -> Word16
value m (Reg r) = M.findWithDefault 0 r m
value m (Const n) = n

applyLine :: Ord r => Line r -> PlayState r -> PlayState r
applyLine (Line _ _ act _) s = foldl' go s act
  where
    go s (Neg r) = M.insert r (neg (s `value` Reg r)) s
    go s (ArithOp o r n) = M.insert r (applyOp o (s `value` Reg r) (s `value` n)) s
    go s (Jump n)  = error "Playing scripts with the J command is not yet implemented. Please file a bug."
    go s _         = s

    neg 0 = 1
    neg _ = 0

    applyOp Inc = (+)
    applyOp Dec = (-)
    applyOp Mult = (*)
    applyOp Div = div
    applyOp Mod = mod
    applyOp And = (.&.)
    applyOp Or  = (.|.)
    applyOp XOr = xor
    applyOp Set = \_ v -> v



forEachNumber :: s -> (Int -> s -> IO s) -> IO ()
forEachNumber state action = go state
  where
    go s = do
        putStrLn "Next OID touched? "
        str <- getLine
        case readMaybe str of
            Just i -> action i s >>= go
            Nothing -> do
                putStrLn "Not a number, please try again"
                go s

