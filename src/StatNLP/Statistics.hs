{-# LANGUAGE RankNTypes #-}


module StatNLP.Statistics
    ( OnlineSummaryState
    , summaryStatsC
    , summaryStatsByC
    , completeSummaryStats
    , zScore
    , tTest
    , tTestDifferences
    , likelihoodRatio
    ) where


import           Conduit
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M

import           StatNLP.Types


data OnlineSummaryState = OSS
                        { ossN    :: !Int
                        , ossMean :: !Double
                        , ossM2   :: !Double
                        }

instance Monoid OnlineSummaryState where
    mempty = OSS 0 0.0 0.0
    mappend (OSS na xa m2a) (OSS nb xb m2b) =
        OSS n (xa + theta * nb' / n') (m2a + m2b + theta^2 * (na' * nb') / n')
        where
            n     = na + nb
            na'   = fromIntegral na
            nb'   = fromIntegral nb
            n'    = fromIntegral n
            theta = xb - xa

summaryStatsC :: Monad m => Consumer Double m OnlineSummaryState
summaryStatsC = foldlC step mempty

summaryStatsByC :: (Eq b, Hashable b, Monad m)
                => (a -> b) -> (a -> Double)
                -> Consumer a m (M.HashMap b OnlineSummaryState)
summaryStatsByC key value = foldlC (step' key value) mempty
    where
        step' key value hm a =
            M.insert k (step (fold $ M.lookup k hm) (value a)) hm
            where
                k = key a

step :: OnlineSummaryState -> Double -> OnlineSummaryState
step (OSS n x_ m) x = OSS n' x_' m'
    where
        n'  = succ n
        n'' = fromIntegral n'
        d   = x - x_
        x_' = x_ + d / n''
        m'  = m + d * (x - x_')

completeSummaryStats :: OnlineSummaryState -> Maybe SummaryStats
completeSummaryStats (OSS n x' m)
    | n < 2     = Nothing
    | otherwise = Just . SummaryStats n x' $ m / (fromIntegral n - 1.0)

zScore :: Double -> Double -> Double -> Double
zScore m sd x = (x - m) / sd

tTest :: Double     -- ^ Expected mean.
      -> Double     -- ^ Actual mean.
      -> Double     -- ^ Sample standard deviation
      -> Int        -- ^ Count
      -> Double     -- ^ Results of the t-test
tTest x_ expected stddev n = (x_ - expected) / (stddev / sqrt (fromIntegral n))

-- | Uses a t-test to compare the whether the word /w/ collocates with one word
-- (/v1/) more than another (/v2/).
tTestDifferences :: Int     -- ^ The count for /w/ and /v1/.
                 -> Int     -- ^ The count for /w/ and /v2/.
                 -> Double  -- ^ The results of the t-test.
tTestDifferences countV1 countV2 = (v1 - v2) / sqrt (v1 + v2)
    where
        v1 = fromIntegral countV1
        v2 = fromIntegral countV2

likelihoodRatio :: Double       -- ^ Probability of /w1/.
                -> Int          -- ^ Frequency of /w1/.
                -> Double       -- ^ Probability of /w2/.
                -> Int          -- ^ Frequency of /w2/.
                -> Double       -- ^ Probability of /w1 w2/.
                -> Int          -- ^ Frequency of /w1 w2/.
                -> Int          -- ^ N
                -> Double       -- ^ The log likelihood ratio.
likelihoodRatio p1 c1 p2 c2 p12 c12 n =
    -2 * ( log' (l c12 c1 p12)
         + log' (l (c2 - c12) (n - c1) p12)
         - log' (l c12 c1 p1)
         - log' (l (c2 - c12) (n - c1) p2))
    where
        l k n x = x^k * (1 - x)^(n - k)

log' :: Floating x => x -> x
log' = logBase 2
