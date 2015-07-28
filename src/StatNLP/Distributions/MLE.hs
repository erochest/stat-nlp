{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module StatNLP.Distributions.MLE
    ( MLEDist
    ) where


import           Control.Arrow                    ((&&&))
import           Control.Monad.Primitive
import           Data.Bifunctor
import           Data.Data
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Lazy                as ML
import qualified Data.HashMap.Strict              as M
import qualified Data.List                        as L
import           Data.List.NonEmpty               hiding (insert)
import           Data.Ord
import           Data.Semigroup
import           Data.Typeable
import qualified Data.Vector.Generic              as GV
import           GHC.Generics
import           GHC.Word                         (Word32)
import           System.Random.MWC
import           System.Random.MWC.CondensedTable

import           StatNLP.Statistics               (mle)
import           StatNLP.Types
import           StatNLP.Utils


data MLEDist s t
        = MLEDist
        { mleRaw    :: !(FreqMap s)
        , mleFreq   :: !(ML.HashMap s Double)
        , mleShrink :: s -> t
        }

mleDist :: (Eq s, Hashable s, Eq t, Hashable t)
        => (s -> t) -> FreqMap s -> MLEDist s t
mleDist shrink sfreqs =
    MLEDist sfreqs ( ML.fromList $ fmap (uncurry step . fmap getSum) sfList) shrink
    where
        sfList = M.toList $ unHash sfreqs
        tfreqs = M.fromListWith mappend $ fmap (first shrink) sfList
        step s sfreq = (s, mle sfreq . lookup' tfreqs $ shrink s)

merge :: (Eq a, Hashable a, Eq b, Hashable b)
      => MLEDist a b -> MLEDist a b -> MLEDist a b
merge (MLEDist a _ f) (MLEDist b _ _) = mleDist f (a `mappend` b)


instance (Eq a, Hashable a, Eq b, Hashable b) => Semigroup (MLEDist a b) where
    (<>) = merge

    sconcat (a :| as) = foldl' merge a as

    times1p 1 a = a
    times1p n a = a <> times1p (n - 1) a

instance (Eq s, Hashable s) => Probabilistic (MLEDist s t) s where
    probability (MLEDist _ fqs _) s = ML.lookupDefault 0.0 s fqs

instance (Eq s, Hashable s) => ProbabilityDist (MLEDist s t) s where
    maxProbability (MLEDist _ fq _) =
        case ML.toList fq of
            [] -> Nothing
            ps -> Just . fst $ L.maximumBy (comparing snd) ps

    samples (MLEDist _ fq _) = ML.keys fq

    -- | Create a condensed look-up table for this distribution.
    table d = tableFromProbabilities
            . GV.fromList
            . ML.toList
            $ mleFreq d
