module Lint (lintTipToi, warnTipToi) where

import Text.Printf
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

import Types
import PrettyPrint

lintTipToi :: TipToiFile -> Segments -> IO ()
lintTipToi tt segments = do
    let hyps = [ (hyp1, "play indicies are correct")
               , (hyp2 (fromIntegral (length (ttAudioFiles tt))),
                  "media indicies are correct")
               ]
    forM_ hyps $ \(hyp, desc) -> do
        let wrong = filter (not . hyp) (concat (mapMaybe snd (ttScripts tt)))
        if null wrong
        then printf "All lines do satisfy hypothesis \"%s\"!\n" desc
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> do
                printf "    %s\n" (ppLine M.empty line)

    let overlapping_segments =
            filter (\((o1,l1,_),(o2,l2,_)) -> o1+l1 > o2) $
            zip segments (tail segments)
    unless (null overlapping_segments) $ do
        printf "Overlapping segments: %d\n"
            (length overlapping_segments)
        mapM_ (uncurry report) overlapping_segments
  where
    hyp1 :: Line ResReg -> Bool
    hyp1 (Line _ _ as mi) = all ok as
      where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
            ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                         0 <= b && b < fromIntegral (length mi)
            ok _ = True

    hyp2 :: Word16 -> Line ResReg -> Bool
    hyp2 n (Line _ _ _ mi) = all (<= n) mi

    report :: Segment -> Segment -> IO ()
    report (o1,l1,d1) (o2,l2,d2)
        | l1 == l2 && o1 == o2
        = printf "   Offset %08X Size %d referenced by %s and %s\n"
            o1 l1 (ppDesc d1) (ppDesc d2)
        | otherwise
        = printf "   Offset %08X Size %d (%s) overlaps Offset %08X Size %d (%s) by %d\n"
            o1 l1 (ppDesc d1) o2 l2 (ppDesc d2) overlap
      where overlap = o1 + l1 - o2

warnTipToi :: TipToiFile -> IO ()
warnTipToi tt = do
    sequence_
      [ printf "[Script %d line %d] Warning: More than 8 commands per line may cause problems\n"
            c n
      | (c, Just lines) <- ttScripts tt
      , (n,Line _ _ cmds _) <- zip [(1::Int)..] lines
      , length cmds > 8
      ]
