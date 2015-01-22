{-# LANGUAGE NondecreasingIndentation #-}

module TextToSpeech where

import System.Directory
import System.IO
import System.FilePath
import System.Process
import System.Exit
import Control.Monad
import Data.Hashable
import Text.Printf

ttsFileName txt = "tts-cache" </> "tts-" ++ map go (shorten txt) <.> "ogg"
  where go '/'         = '_'
        go c | c < ' ' = '_'
        go c           = c
        shorten x | length x > 20 = printf "%s-%016X" (take 20 x) (hash x)
        shorten x                 = x

textToSpeech :: FilePath -> String -> IO ()
textToSpeech fn txt = do
    ex <- doesFileExist fn
    if ex then return () else do

    createDirectoryIfMissing True (takeDirectory fn)

    putStrLn $ "Speaking \"" ++ txt ++ "\"."  
    (tmp,h) <- openTempFile (takeDirectory fn) (takeBaseName fn <.> "wav")
    hClose h

    (ret, _, err) <- readProcessWithExitCode "pico2wave" ["--wave", tmp, "--lang", "en-GB", txt] ""
    unless (ret == ExitSuccess) $ do
        putStrLn "Failed to execute \"pico2wave\":"
        putStrLn err
        putStrLn "Do you have libttspico-utils installed?"

    (ret, _, err) <- readProcessWithExitCode "oggenc" ["-o", fn, tmp] ""
    unless (ret == ExitSuccess) $ do
        putStrLn "Failed to execute \"oggenc\":"
        putStrLn err
        putStrLn "Do you have vorbis-tools installed?"

    removeFile tmp
    return ()
