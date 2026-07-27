module PlaySound (playSound) where

import qualified Data.ByteString.Lazy.Char8 as B

import Sound.ProteaAudio


sampleFromMemory :: B.ByteString -> Float -> IO Sample
sampleFromMemory bs
  | B.take 4 bs == B.pack "RIFF" = sampleFromMemoryWav (B.toStrict bs)
  | B.take 4 bs == B.pack "OggS" = sampleFromMemoryOgg (B.toStrict bs)
  | B.take 3 bs == B.pack "ID3" || B.take 2 bs `elem`
      [B.pack "\xFF\xFB", B.pack "\xFF\xF3", B.pack "\xFF\xF2"]
  = sampleFromMemoryMp3 (B.toStrict bs)
  | otherwise = error "Could not detect audio format"
 where magic = B.take 4 bs


playSound :: B.ByteString -> IO ()
playSound content = do
    _ok <- initAudio 1 22050 512
    soundStopAll
    sample <- sampleFromMemory content 1
    _sound <- soundPlay sample 1 1 0 1
    -- finishAudio
    return ()
