{-# LANGUAGE FlexibleContexts #-}
module GMERun (playTipToi, execOID) where

import Text.Printf
import Data.Bits
import Data.List
import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Reader
import System.Console.Haskeline
import System.Directory
import System.FilePath
import Data.Foldable (for_)

import Types
import PrettyPrint
import Utils
import PlaySound

type PlayState r = M.Map r Word16

type GMEM r =
    ReaderT (Transscript, TipToiFile) (StateT (PlayState r) IO)

formatState :: PlayState ResReg -> String
formatState s = spaces $
    map (\(k,v) -> printf "$%d=%d" k v) $
    filter (\(k,v) -> k == 0 || v /= 0) $
    M.toAscList s

playTipToi :: Transscript -> TipToiFile -> IO ()
playTipToi t tt = do
    let initialState = M.fromList $ zip [0..] (ttInitialRegs tt)
    printf "Initial state (not showing zero registers): %s\n" (formatState initialState)
    dir <- getAppUserDataDirectory "tttool"
    createDirectoryIfMissing True dir
    let history_file = dir </> "play_history"
    let haskeline_settings = defaultSettings { historyFile = Just history_file }
    flip evalStateT initialState $
        flip runReaderT (t,tt) $
        runInputT haskeline_settings $
        nextNumber $ \i -> do
                execOID i
                s <- get
                liftIO $ printf "State now: %s\n" $ formatState s


execOID :: Word16 -> GMEM Word16 ()
execOID i = do
    (t,tt) <- ask
    case lookup (fromIntegral i) (ttScripts tt) of
        Nothing -> do
            liftIO $ printf "OID %d not in main table\n" i
        Just Nothing -> do
            liftIO $ printf "OID %d deactivated\n" i
        Just (Just lines) -> do
            code <- gets $ \s -> find (enabledLine s) lines
            case code of
                Nothing -> liftIO $ do
                    printf "None of these lines matched!\n"
                    mapM_ (putStrLn . ppLine t) lines

                Just l -> do
                    liftIO $ printf "Executing:  %s\n" (ppLine t l)
                    applyLine l

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

playTTAudio :: Word16 -> GMEM r ()
playTTAudio i = do
    (_,tt) <- ask
    liftIO $ printf "Playing audio sample %d\n" i
    let bs = ttAudioFiles tt !! fromIntegral i
    liftIO $ playSound bs

applyLine :: Line Word16 -> GMEM Word16 ()
applyLine (Line _ _ acts playlist) = go acts
  where
    go (Neg r : acts) = do
        modReg r neg
        go acts
    go (ArithOp o r v : acts) = do
        arg <- getVal v
        let (*) = applyOp o
        modReg r (* arg)
        go acts
    go (Jump v: _ ) = do
        n <- getVal v
        execOID n
    go (Play n: acts) = do
        playTTAudio (playlist !! fromIntegral n)
        go acts
    go (_: acts) = do
        go acts
    go [] = return ()

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


untilNothing :: Monad m => (a -> m (Maybe a)) -> a -> m ()
untilNothing f i = do
    r <- f i
    case r of Just i' -> untilNothing f i'
              Nothing -> return ()

nextNumber :: (MonadIO m, MonadException m) =>  (Word16 -> m ()) -> InputT m ()
nextNumber action = go
  where
    go = do
        mstr <- getInputLine "Next OID touched? "
        for_ mstr $ \str -> do
                case readMaybe str of
                    Just i ->  lift $ action i
                    Nothing -> liftIO $ putStrLn "Not a number, please try again"
                go

