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
import System.Environment
import System.Info (os)
import System.Environment.Executable

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
            Language "en" -> "en-GB"
            Language "de" -> "de-DE"
            Language "fr" -> "fr-FR"
            Language s    -> s

espeak :: Language -> FilePath -> String -> (String, [String])
espeak lang tmp txt =
 ("espeak", ["-v", l, "-w", tmp, "-s", "120", txt])
  where
    l = case lang of
            Language s    -> s

espeak_contrib :: FilePath -> Language -> FilePath -> String -> (String, [String])
espeak_contrib myDir lang tmp txt =
 (myDir </> "contrib" </> "espeak", ["-v", l, "-w", tmp, "-s", "120", txt])
  where
    l = case lang of
            Language s    -> s


engines :: FilePath -> Language -> FilePath -> String -> [(String, [String])]
engines myDir l ft txt =
    [ pico l ft txt
    , espeak l ft txt
    , espeak_contrib myDir l ft txt
    ]

oggenc :: FilePath -> FilePath -> (String, [String])
oggenc from to = ("oggenc", ["-Q", "-o", to, from])

oggenc_contrib :: FilePath -> FilePath -> (String, [String])
oggenc_contrib from to = ("contrib/oggenc", ["-Q", "-o", to, from])

encoders :: FilePath -> FilePath -> [(String, [String])]
encoders from to =
    [ oggenc from to
    , oggenc_contrib from to
    ]

tryPrograms [] e = e
tryPrograms ((c,args):es) e = do
    -- Missing programs cause exceptions on Windows, but error 127 on Linux.
    -- Try to handle both here.
    r <- tryJust (guard . isDoesNotExistError) $ do
        env <- getEnvironment
        let env' | os == "mingw32" = ("ESPEAK_DATA_PATH", "contrib") : env
                 | otherwise       = env
        ph <- runProcess  c args Nothing (Just env') Nothing Nothing Nothing
        ret <- waitForProcess ph
        if ret == ExitSuccess then return True
        else if ret == ExitFailure 127 then return False
        else do
            putStrLn $ "Failed to execute \"" ++ c ++ "\" (" ++ show ret ++ ")"
            exitFailure
    case r of
       Right True -> return ()
       _ -> tryPrograms es e

textToSpeech :: Language -> String -> IO ()
textToSpeech lang txt = do
    ex <- doesFileExist fn
    if ex then return () else do

    createDirectoryIfMissing True (takeDirectory fn)

    putStrLn $ "Speaking \"" ++ txt ++ "\"."  
    (tmp,h) <- openTempFile (takeDirectory fn) (takeBaseName fn <.> "wav")
    hClose h

    (myDir,_) <- splitExecutablePath

    tryPrograms (engines myDir lang tmp txt) $ do
        putStrLn "No suitable text-to-speech-engine found."
        putStrLn "Do you have libttspico-utils or espeak installed?"

    tryPrograms (encoders tmp fn) $ do
        putStrLn "Could not find \"oggenc\"."
        putStrLn "Do you have vorbis-tools installed?"

    removeFile tmp
    return ()
  where
    fn = ttsFileName lang txt
