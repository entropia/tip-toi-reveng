{-# LANGUAGE NondecreasingIndentation #-}

module PlaySound (playSound) where

import System.Process.ByteString
import System.Exit
import Control.Monad
import Control.Exception
import System.IO.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


playSound :: BL.ByteString -> IO ()
playSound content = do
    tryPrograms content ["sox", "./sox/sox"] $ do
        putStrLn "Could not play audio file."
        putStrLn "Do you have \"sox\" installed?"

soxArgs = ["-q", "-", "-d"]

tryPrograms content [] e = e
tryPrograms content (sox:soxes) e = do
    -- Missing programs cause exceptions on Windows, but error 127 on Linux.
    -- Try to handle both here.
    r <- tryJust (guard . isDoesNotExistError) $ do
        (ret,out,err) <- readProcessWithExitCode sox soxArgs (BL.toStrict content)
        if ret == ExitSuccess then return True
        else if ret == ExitFailure 127 then return False
        else do
            putStrLn $ "Failed to execute \"" ++ sox ++ "\" (" ++ show ret ++ "):"
            B.putStrLn err
            exitFailure
    case r of
       Right True -> return ()
       _ -> tryPrograms content soxes e
