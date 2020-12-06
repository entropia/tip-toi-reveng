module Lint (lintTipToi, warnTipToi) where

import Text.Printf
import Data.Maybe
import Control.Monad
import qualified Data.Map as M

import Types
import PrettyPrint

lintTipToi :: TipToiFile -> Segments -> IO Bool
lintTipToi tt segments = do
    let hyps = [ (hyp1, "play indicies are correct")
               , (hyp2, "media indicies are correct")
               , (hyp3, "at most one jump per line, as last action")
               ]
    hyp_result <- forM hyps $ \(hyp, desc) -> do
        let wrong = filter (not . hyp) (concat (mapMaybe snd (ttScripts tt)))
        if null wrong
        then do
            printf "All lines do satisfy hypothesis \"%s\"!\n" desc
            return True
        else do
            printf "These lines do not satisfy hypothesis \"%s\":\n" desc
            forM_ wrong $ \line -> do
                printf "    %s\n" (ppLine M.empty line)
            return False

    media_result <- forM (fromMaybe [] (ttMediaFlags tt)) $ \f ->
        if (f > 1)
        then do
            printf "Media flag >1: %d" f
            return False
        else do
            return True

    let overlapping_segments =
            filter (\((o1,l1,_),(o2,l2,_)) -> o1+l1 > o2) $
            zip segments (tail segments)
    unless (null overlapping_segments) $ do
        printf "Overlapping segments: %d\n"
            (length overlapping_segments)
        mapM_ (uncurry report) overlapping_segments

    return $ and $ hyp_result ++ media_result ++ [null overlapping_segments]
  where
    hyp1 :: Line ResReg -> Bool
    hyp1 (Line _ _ as mi) = all ok as
      where ok (Play n)   = 0 <= n && n < fromIntegral (length mi)
            ok (Random a b) = 0 <= a && a < fromIntegral (length mi) &&
                         0 <= b && b < fromIntegral (length mi)
            ok _ = True

    media_count :: Word16
    media_count  = fromIntegral (length (ttAudioFiles tt))

    hyp2 :: Line ResReg -> Bool
    hyp2 (Line _ _ _ mi) = all (< media_count) mi

    max_one_jump_at_end [] = True
    max_one_jump_at_end (Jump x : acts) = null acts
    max_one_jump_at_end (x : acts) = max_one_jump_at_end acts

    hyp3 :: Line ResReg -> Bool
    hyp3 (Line _ _ as _) = max_one_jump_at_end as

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
