{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}


module StatNLP.Distributions.SimpleGoodTuring
    ( SGTDist
    , sgtDist
    , merge
    ) where


import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict              as M
import qualified Data.List                        as L
import           Data.List.NonEmpty               hiding (insert)
import           Data.Ord
import           Data.Semigroup
import qualified Data.Vector.Generic              as GV
import           System.Random.MWC
import           System.Random.MWC.CondensedTable

import           StatNLP.Statistics               (sgt)
import           StatNLP.Text.Freqs
import           StatNLP.Types
import           StatNLP.Utils


data SGTDist s
        = SGTDist
        { sgtBase       :: !(FreqMap s)
        , sgtConfidence :: !Double
        , sgtFreqs      :: !(M.HashMap Int (Double, Double))
        }

sgtDist :: FreqMap a -> Double -> SGTDist a
sgtDist freqs confLevel =
    SGTDist freqs confLevel
        . uncurry (flip (M.insert 0 . (0,)))
        . (`sgt` confLevel)
        $ countCounts freqs

merge :: (Eq s, Hashable s) => SGTDist s -> SGTDist s -> SGTDist s
merge (SGTDist a cl _) (SGTDist b _ _) = sgtDist (a `mappend` b) cl

instance (Eq s, Hashable s) => Semigroup (SGTDist s) where
    (<>) = merge

    sconcat (a :| as) = foldl' merge a as

    times1p 1 a = a
    times1p n a = a <> times1p (n - 1) a

instance (Eq s, Hashable s) => ProbabilityDist (SGTDist s) s where
        probability (SGTDist (MHash fqs) _ p) s =
            snd $ lookupP p (lookup' fqs s)

        maxProbability (SGTDist (MHash fqs) _ p) =
            case M.toList fqs of
                [] -> Nothing
                xs -> Just
                   .  fst
                   .  L.maximumBy (comparing snd)
                   .  fmap (fmap (snd . lookupP p . getSum))
                   $  M.toList fqs

        samples (SGTDist (MHash fqs) _ _) = M.keys fqs

        discount (SGTDist fqs _ p) =
            1.0 * fst (lookupP p 1) * fromIntegral (grandTotal fqs)

        table (SGTDist (MHash fqs) _ p) =
            tableFromProbabilities
                . GV.fromList
                . fmap (fmap (snd . lookupP p . getSum))
                $ M.toList fqs

lookupP :: M.HashMap Int (Double, Double) -> Int -> (Double, Double)
lookupP m i = M.lookupDefault (0, 0) i m
