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
import Control.Exception
import System.IO.Error

import Language

ttsFileName lang txt =
    "tts-cache" </> "tts-" ++ map go (shorten txt) ++ "-" ++ ppLang lang <.>  "ogg"
  where go '/'         = '_'
        go c | c < ' ' = '_'
        go c           = c
        shorten x | length x > 20 = printf "%s-%016X" (take 20 x) (hash x)
        shorten x                 = x


pico :: Language -> FilePath -> String -> (String, [String])
pico lang tmp txt =
   ("pico2wave", ["--wave", tmp, "--lang", l, txt])
  where
    l = case lang of 
            En -> "en-GB"
            De -> "de-DE"
            Fr -> "fr-FR"

espeak :: Language -> FilePath -> String -> (String, [String])
espeak lang tmp txt =
 ("espeak", ["-v", l, "-w", tmp, "-s", "120", txt])
  where
    l = case lang of 
            En -> "en"
            De -> "de"
            Fr -> "fr"

engines :: Language -> FilePath -> String -> [(String, [String])]
engines l ft txt =
    [ pico l ft txt
    , espeak l ft txt
    ]

createWav [] = do
    putStrLn "No suitable text-to-speech-engine found."
    putStrLn "Do you have libttspico-utils or espeak installed?"
    exitFailure
createWav ((c,args):es) = do
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
       _ -> createWav es 

textToSpeech :: Language -> String -> IO ()
textToSpeech lang txt = do
    ex <- doesFileExist fn
    if ex then return () else do

    createDirectoryIfMissing True (takeDirectory fn)

    putStrLn $ "Speaking \"" ++ txt ++ "\"."  
    (tmp,h) <- openTempFile (takeDirectory fn) (takeBaseName fn <.> "wav")
    hClose h

    createWav $ engines lang tmp txt

    (ret, _, err) <- readProcessWithExitCode "oggenc" ["-o", fn, tmp] ""
    unless (ret == ExitSuccess) $ do
        putStrLn "Failed to execute \"oggenc\":"
        putStrLn err

    removeFile tmp
    return ()
  where
    fn = ttsFileName lang txt
