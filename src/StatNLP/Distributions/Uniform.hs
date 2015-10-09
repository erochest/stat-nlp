{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}


module StatNLP.Distributions.Uniform
    ( UniformDist
    , uniformDist
    , uniformDistFromSet
    , insert
    , size
    , singleton
    , merge
    ) where


import           Data.Data
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashSet                     as S
import           Data.List.NonEmpty               hiding (insert)
import           Data.Maybe
import           Data.Semigroup
import           Data.Vector                      ((!), (!?))
import qualified Data.Vector                      as V
import           GHC.Generics
import           System.Random.MWC
import           System.Random.MWC.CondensedTable

import           StatNLP.Types


data UniformDist s
        = UniformDist
        { uniformSet     :: !(S.HashSet s)
        , uniformProb    :: !Double
        , uniformSamples :: !(V.Vector s)
        } deriving (Show, Data, Typeable, Generic)

uniformDist :: (Foldable t, Eq s, Hashable s) => t s -> Maybe (UniformDist s)
uniformDist xs = uniformDistFromSet $ foldl' (flip S.insert) S.empty xs

uniformDistFromSet :: S.HashSet s -> Maybe (UniformDist s)
uniformDistFromSet s
    | S.null s  = Nothing
    | otherwise = Just
                . UniformDist s (1.0 / fromIntegral (S.size s))
                . V.fromList
                $ S.toList s

insert :: (Eq s, Hashable s) => s -> UniformDist s -> UniformDist s
insert x u@(UniformDist s _ _) =
    if S.size s == S.size s'
        then u
        else fromMaybe (singleton x) $ uniformDistFromSet s'
    where
        s' = S.insert x s

singleton :: (Eq s, Hashable s) => s -> UniformDist s
singleton s = UniformDist (S.singleton s) 1.0 (V.singleton s)

size :: UniformDist s -> Int
size = S.size . uniformSet

merge :: (Eq s, Hashable s) => UniformDist s -> UniformDist s -> UniformDist s
merge a b = fromMaybe a . uniformDistFromSet $ uniformSet a <> uniformSet b

instance (Eq a, Hashable a) => Semigroup (UniformDist a) where
    (<>) = merge

    sconcat (a :| as) = foldl' merge a as

    times1p 1 a = a
    times1p n a = a <> times1p (n - 1) a

instance (Eq s, Hashable s) => Probabilistic (UniformDist s) s where
        probability d _    = uniformProb d
        logProbability d _ = Just . (`logBase` 2) $ uniformProb d

instance (Eq s, Hashable s) => ProbabilityDist (UniformDist s) s where
        maxProbability d   = uniformSamples d !? 0
        samples            = V.toList . uniformSamples
        generate d gen     = let xs = uniformSamples d
                             in  (xs !) <$> uniformR (0, V.length xs - 1) gen
        table              = tableFromIntWeights
                           . V.convert
                           . fmap (,1)
                           . uniformSamples
