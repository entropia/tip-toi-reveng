{-# LANGUAGE NondecreasingIndentation #-}

module PlaySound (withSoundPlayer) where

import Control.Monad
import System.IO
import System.Directory
import Data.Foldable
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Foreign.ForeignPtr

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

withSoundPlayer :: ((B.ByteString -> IO ()) -> IO a ) -> IO a
withSoundPlayer body = bracket_ init cleanup (body playSound)
  where
    init = do
        SDL.init [SDL.InitAudio]
        getError >>= traverse_ putStrLn
        ok <- Mix.tryOpenAudio Mix.defaultFrequency Mix.AudioS16LSB 2  4096
        unless ok $
            putStrLn "Failed to open SDL audio device"

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

    cleanup = do
        Mix.closeAudio
        SDL.quit


wait :: IO ()
wait = do
    SDL.delay 50
    stillPlaying <- Mix.playingMusic
    when (stillPlaying) wait
