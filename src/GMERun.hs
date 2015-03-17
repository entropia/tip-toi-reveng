{-# LANGUAGE FlexibleContexts #-}
module GMERun (playTipToi) where

import Text.Printf
import Data.Bits
import Data.List
import qualified Data.Map as M
import Control.Monad.State

import Types
import PrettyPrint
import Utils

type PlayState r = M.Map r Word16

type GMEM r = StateT (PlayState r) IO

formatState :: PlayState ResReg -> String
formatState s = spaces $
    map (\(k,v) -> printf "$%d=%d" k v) $
    filter (\(k,v) -> k == 0 || v /= 0) $
    M.toAscList s

playTipToi :: Transscript -> TipToiFile -> IO ()
playTipToi t tt = do
    let initialState = M.fromList $ zip [0..] (ttInitialRegs tt)
    printf "Initial state (not showing zero registers): %s\n" (formatState initialState)
    flip evalStateT initialState $ forEachNumber $ \i -> do
        case lookup (fromIntegral i) (ttScripts tt) of
            Nothing -> lift $ printf "OID %d not in main table\n" i
            Just Nothing -> lift $ printf "OID %d deactivated\n" i
            Just (Just lines) -> do
                code <- gets $ \s -> find (enabledLine s) lines
                case code of
                    Nothing -> lift $ do
                        printf "None of these lines matched!\n"
                        mapM_ (putStrLn . ppLine t) lines
                    Just l -> do
                        lift $ printf "Executing:  %s\n" (ppLine t l)
                        applyLine l
                        get >>= lift . printf "State now: %s\n" . formatState

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

getVal :: (Ord r, MonadState (PlayState r) m) => TVal r -> m Word16
getVal v = gets $ \s ->  value s v

modReg :: (Ord r, MonadState (PlayState r) m) => r -> (Word16 -> Word16) -> m ()
modReg r f = do
    x <- getVal (Reg r)
    modify $ M.insert r (f x)

applyLine :: (Ord r, MonadState (PlayState r) m) => Line r -> m ()
applyLine (Line _ _ act _) = mapM_ go act
  where
    go (Neg r) = modReg r neg
    go (ArithOp o r n) = do
        arg <- getVal n
        let (*) = applyOp o
        modReg r (* arg)
    go (Jump n) = error "Playing scripts with the J command is not yet implemented. Please file a bug."
    go _        = return ()

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



forEachNumber :: (Int -> StateT s IO ()) -> StateT s IO ()
forEachNumber action = forever $ do
    lift $ putStrLn "Next OID touched? "
    str <- lift $ getLine
    case readMaybe str of
        Just i ->  action i
        Nothing -> lift $ putStrLn "Not a number, please try again"

