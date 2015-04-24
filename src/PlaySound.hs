module PlaySound (withSound, playSound) where

import Control.Monad
import System.IO
import System.Directory
import Data.Foldable
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Foreign.ForeignPtr

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

withSound :: IO a -> IO a
withSound = bracket_ init cleanup
  where
    init = do
        SDL.init [SDL.InitAudio]
        getError >>= traverse_ putStrLn
        ok <- Mix.tryOpenAudio Mix.defaultFrequency Mix.AudioS16LSB 2  4096
        unless ok $
            putStrLn "Failed to open SDL audio device"

    cleanup = do
        Mix.closeAudio
        SDL.quit

playSound :: B.ByteString -> IO ()
playSound content = do
        dir <- getTemporaryDirectory
        (tmp, h) <- openTempFile dir "sdl-input"
        B.hPutStr h content
        hClose h

        mus <- Mix.loadMUS tmp
        Mix.playMusic mus 1
        wait

        -- This would double-free the Music, as it is also freed via a
        -- finalizer
        --Mix.freeMusic mus
        finalizeForeignPtr mus
        removeFile tmp

wait :: IO ()
wait = do
    SDL.delay 50
    stillPlaying <- Mix.playingMusic
    when (stillPlaying) wait
