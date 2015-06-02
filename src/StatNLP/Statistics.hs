{-# LANGUAGE RankNTypes #-}


module StatNLP.Statistics
    ( OnlineSummaryState
    , summaryStatsC
    , summaryStatsByC
    , completeSummaryStats
    , zScore
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
