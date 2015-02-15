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

import Language

ttsFileName lang txt = "tts-cache" </> "tts-" ++ map go (shorten txt) ++ "-" ++ ppLang lang <.>  "ogg"
  where go '/'         = '_'
        go c | c < ' ' = '_'
        go c           = c
        shorten x | length x > 20 = printf "%s-%016X" (take 20 x) (hash x)
        shorten x                 = x

langToPico2WaveParam En = "en-GB"
langToPico2WaveParam De = "de-DE"
langToPico2WaveParam Fr = "fr-FR"

textToSpeech :: Language -> String -> IO ()
textToSpeech lang txt = do
    ex <- doesFileExist fn
    if ex then return () else do

    createDirectoryIfMissing True (takeDirectory fn)

    putStrLn $ "Speaking \"" ++ txt ++ "\"."  
    (tmp,h) <- openTempFile (takeDirectory fn) (takeBaseName fn <.> "wav")
    hClose h

    (ret, _, err) <- readProcessWithExitCode "pico2wave" ["--wave", tmp, "--lang", langToPico2WaveParam lang, txt] ""
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
  where
    fn = ttsFileName lang txt
