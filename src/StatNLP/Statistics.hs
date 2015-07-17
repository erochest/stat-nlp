{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}


module StatNLP.Statistics
    ( OnlineSummaryState
    , summaryStatsC
    , summaryStatsByC
    , completeSummaryStats
    , zScore
    , tTest
    , tTestDifferences
    , likelihoodRatio
    , log'
    , pointwiseMI
    , pointwiseMI'
    , mle
    , sgt
    ) where


import           Conduit
import           Control.Arrow         ((&&&))
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict   as M
import qualified Data.List             as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Traversable
import           Data.Vector.Unboxed   ((!), (!?))
import qualified Data.Vector.Unboxed   as V
import           Statistics.Matrix     hiding (map)
import           Statistics.Regression (ols)
import           Statistics.Sample     (mean)

import           StatNLP.Text.Freqs
import           StatNLP.Types
import           StatNLP.Utils

import           Debug.Trace


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

-- | Calculates the pointwise mutual information of the pair /x y/, with
-- /y | x/
pointwiseMI :: Double -> Double -> Double
pointwiseMI x y = log' $ y / (x * y)

pointwiseMI' :: Int -> Int -> Int -> Double
pointwiseMI' x y n = pointwiseMI (fromIntegral x / n') (fromIntegral y / n')
    where
        n' = fromIntegral n

mle :: Int      -- ^ /C(w1 .. wn)/
    -> Int      -- ^ /C(w1 .. w(n-1))/
    -> Double   -- ^ maximum likelihood estimate
mle cn cn1 = fromIntegral cn / fromIntegral cn1

-- | Calculate the Simple Good Turing estimator for all frequencies.
sgt :: FreqMap Int      -- ^ A frequency map to generate generate probs for.
    -> Double           -- ^ Multiplier of the std dev (1.96 is a good default).
    -> (M.HashMap Int Double, Double)
sgt counts@(MHash counts') confLevel = (pmap, pZero)
    where
        -- (r, n)
        rn :: V.Vector (Int, Int)
        rn = V.fromList
           . L.sortBy (comparing fst)
           . fmap (fmap getSum)
           . M.toList
           $ unHash counts

        -- roughly, n[row(i)]
        n :: Int -> Int
        n = lookup' counts'

        rd :: (Int, Int) -> Double
        rd = fromIntegral . fst

        logs :: Int -> (Int, Int) -> (Double, Double, Double)
        logs j (r, n) =
            let i  = if j == 0 then 0 else fst (rn ! (j - 1))
                i' = fromIntegral i
                k  = maybe (2.0 * rd (rn ! j) - i') rd
                   $ rn !? (j + 1)
                z  = 2.0 * fromIntegral n / (k - i')
            in  ( z
                , log $ fromIntegral r
                , log z
                )

        smoothed :: Double -> Double
        smoothed = exp . (intercept +) . (slope *) . log

        getRstar :: (Bool, Double) -> (Int, (Int, Int)) -> (Bool, Double)
        getRstar (indiffValsSeen, _) (j, (r, c)) =
            let rj = rd $ rn ! j
                r' = fromIntegral r
                c' = fromIntegral c
                y  = rj + 1 * smoothed (rj + 1) / smoothed rj
                indiffValsSeen' = indiffValsSeen || not (M.member (r+1) counts')
            in  if indiffValsSeen'
                    then (indiffValsSeen', y)
                    else let nextN = fromIntegral . n $ r + 1
                             x = fromIntegral (r + 1) * nextN / fromIntegral c
                             cutOff = confLevel
                                    * sqrt((r' + 1.0)^2)
                                    * nextN
                                    / ( c'^2 * (1 + nextN / c'))
                         in  if abs (x - y) <= confLevel * sqrt ((r' + 1.0)^2)
                                 then (True, y)
                                 else (indiffValsSeen', x)

        zlrz :: V.Vector (Double, Double, Double)
        z, logR, logZ, rStar, p :: Vector
        zlrz  = V.imap logs rn
        z     = V.map (\(x, _, _) -> x) zlrz
        logR  = V.map (\(_, x, _) -> x) zlrz
        logZ  = V.map (\(_, _, x) -> x) zlrz
        rStar = V.map snd . V.postscanl' getRstar (False, 0.0) $ V.indexed rn

        p     = V.map (\rs -> (1 - pZero) * rs / bigNprime) rStar

        rows, bigN :: Int
        rows = V.length rn
        bigN = V.sum $ V.map (uncurry (*)) rn

        pZero, bigNprime, slope, intercept :: Double
        pZero     = fromIntegral (n 1) / fromIntegral bigN
        bigNprime = V.sum . V.zipWith (*) rStar $ V.map (fromIntegral . snd) rn

        -- findBestFit
        (slope, intercept) =
            let meanX = mean logR
                meanY = mean logZ
                rdiff = V.map (\z -> z - meanX) logR
                xys   = V.sum
                      . V.zipWith (*) rdiff
                      $ V.map (\z -> z - meanY) logZ
                xsq   = V.sum $ V.map (^2) rdiff
            in  (xys / xsq, meanY - slope * meanX)

        pmap :: M.HashMap Int Double
        pmap = M.fromList . V.toList $ V.zipWith ((,) . fst) rn p
