{-# LANGUAGE NondecreasingIndentation #-}

module PlaySound (playSound) where

import System.Process
import System.Exit
import Control.Monad
import Control.Exception
import System.IO.Error
import System.IO
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as B
import System.Environment.Executable


players :: FilePath -> FilePath -> [(FilePath, [String])]
players myDir fn =
    [ ("sox",                              ["-q", fn, "-d"])
    , (myDir </> "contrib" </> "playmus",  [fn])
    ]

playSound :: B.ByteString -> IO ()
playSound content = do
    dir <- getTemporaryDirectory
    (tmp, h) <- openTempFile dir "tttool-audio.tmp"
    B.hPutStr h content
    hClose h

    (myDir,_) <- splitExecutablePath

    tryPrograms (players myDir tmp) $ do
        putStrLn "Could not play audio file."
        putStrLn "Do you have \"sox\" installed?"

    removeFile tmp

tryPrograms [] e = e
tryPrograms ((c,args):es) e = do
    -- Missing programs cause exceptions on Windows, but error 127 on Linux.
    -- Try to handle both here.
    r <- tryJust (guard . isDoesNotExistError) $ do
        ph <- runProcess  c args Nothing Nothing Nothing Nothing Nothing
        ret <- waitForProcess ph
        if ret == ExitSuccess then return True
        else if ret == ExitFailure 127 then return False
        else do
            putStrLn $ "Failed to execute \"" ++ c ++ "\" (" ++ show ret ++ ")"
            exitFailure
    case r of
       Right True -> return ()
       _ -> tryPrograms es e
