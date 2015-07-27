{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module StatNLP.Distributions.Map
    ( MapProbDist
    , mapProbDist
    , size
    , merge
    ) where


import           Data.Data
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict              as M
import qualified Data.List                        as L
import           Data.List.NonEmpty               hiding (insert)
import           Data.Ord
import           Data.Semigroup
import           Data.Typeable
import qualified Data.Vector.Generic              as GV
import           GHC.Generics
import           System.Random.MWC
import           System.Random.MWC.CondensedTable

import           StatNLP.Types


data MapProbDist s
        = MapProbDist
        { mapProb :: !(M.HashMap s Double)
        } deriving (Show, Data, Typeable, Generic)

mapProbDist :: M.HashMap s Double -> MapProbDist s
mapProbDist m = MapProbDist $ fmap (/total) m
    where
        total = sum $ M.elems m

size :: MapProbDist s -> Int
size = M.size . mapProb

merge :: (Eq s, Hashable s) => MapProbDist s -> MapProbDist s -> MapProbDist s
merge (MapProbDist a) (MapProbDist b) = mapProbDist $ M.unionWith (+) a b

instance (Eq a, Hashable a) => Semigroup (MapProbDist a) where
    (<>) = merge

    sconcat (a :| as) = foldl' merge a as

    times1p 1 a = a
    times1p n a = a <> times1p (n - 1) a

instance (Eq s, Hashable s) => ProbabilityDist (MapProbDist s) s where
    probability (MapProbDist d) o = M.lookupDefault 0.0 o d
    maxProbability (MapProbDist d) =
        case M.toList d of
            [] -> Nothing
            ds -> Just . fst $ L.maximumBy (comparing snd) ds
    samples (MapProbDist d) = M.keys d
    table d = tableFromProbabilities
            . GV.fromList
            . M.toList
            $ mapProb d
