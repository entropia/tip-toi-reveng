module PlaySound (playSound) where

import qualified Data.ByteString.Lazy as B

import Sound.ProteaAudio

playSound :: B.ByteString -> IO ()
playSound content = do
    _ok <- initAudio 1 22050 512
    soundStopAll
    sample <- sampleFromMemoryOgg (B.toStrict content) 1
    _sound <- soundPlay sample 1 1 0 1
    -- finishAudio
    return ()
