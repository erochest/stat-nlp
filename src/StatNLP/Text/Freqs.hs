{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Freqs where


import           Conduit
import           Control.Arrow       hiding (first)
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Foldable
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Traversable
import           Prelude             hiding (lookup)

import           StatNLP.Types


hapaxLegomena :: (Eq a, Hashable a) => FreqMap a -> S.HashSet a
hapaxLegomena = S.fromList . map fst . filter ((==1) . snd) . M.toList . unHash

replaceFromSet :: (Eq a, Hashable a, Traversable t)
               => S.HashSet a -> a -> t a -> t a
replaceFromSet set with = fmap repl
    where
        repl a | a `S.member` set = with
               | otherwise        = a

replaceFreqsFromSet :: (Eq a, Hashable a)
                    => S.HashSet a -> a -> FreqMap a -> FreqMap a
replaceFreqsFromSet set with = MHash
                             . M.fromListWith mappend
                             . fmap repl
                             . M.toList
                             . unHash
    where
        repl p@(a, c) | a `S.member` set = (with, c)
                      | otherwise        = p

reconstitute :: FreqMap a -> [a]
reconstitute = concatMap (uncurry (flip L.replicate) . fmap getSum)
             . M.toList
             . unHash

-- | Return frequencies of groups of input.
conditionalFreqs :: (Eq k, Hashable k, Eq v, Hashable v, Foldable f)
                 => (a -> k) -> (a -> v) -> f a -> ConditionalFreq k v
conditionalFreqs by value =
    foldMap ( MHash
            . uncurry M.singleton
            . (by &&& (MHash . (`M.singleton` 1) . value)))

conditions :: ConditionalFreq k v -> [k]
conditions = M.keys . unHash

-- | Take a frequency map and generate frequencies of subgroups from it.
groupFreqsBy :: (Ord k, Eq k, Hashable k, Eq v, Hashable v)
             => (a -> k) -> (a -> v) -> FreqMap a -> ConditionalFreq k v
groupFreqsBy key val = fromList
                     . mapMaybe ( bisequenceA
                                . (getFirst `bimap` (Just . fromList))
                                . sequenceA
                                . fmap (first (First . Just))
                                )
                     . L.groupBy ((==) `on` fst)
                     . L.sortBy (compare `on` fst)
                     . fmap (\(a, b) -> (key a, (val a, b)))
                     . M.toList
                     . unHash
    where
        fromList :: (Eq k, Hashable k, Monoid m) => [(k, m)] -> MonoidHash k m
        fromList = MHash . M.fromListWith mappend

frequencies :: (Eq a, Hashable a, Traversable t) => t a -> FreqMap a
frequencies = countAll mempty

frequenciesC :: (Eq a, Hashable a, Monad m) => Consumer a m (FreqMap a)
frequenciesC = foldlC count mempty

grandTotal :: FreqMap a -> Int
grandTotal = getSum . fold . M.elems . unHash

conditionalTotal :: ConditionalFreq a b -> Int
conditionalTotal = getSum . mconcat . foldMap freqElems . freqElems
    where
        freqElems = M.elems . unHash

freqsFromConditionals :: (Eq b, Hashable b) => ConditionalFreq a b -> FreqMap b
freqsFromConditionals = fold . M.elems . unHash

-- | Return the number of bins (sample values) with counts.
bins :: FreqMap a -> Int
bins = M.size . unHash

-- | Return all items in the sample that only occur once.
hapaxes :: FreqMap a -> [a]
hapaxes = M.keys . M.filterWithKey (const (==1)) . unHash

-- | Return the count counts.
--
-- If @b@ (bins) is given, it's used to calculate @Nr(0)@; if @Nothing@,
-- @Nr(0) = 0@.
rNr :: Maybe Int -> FreqMap a -> FreqMap Int
rNr b freqs = MHash . M.insert 0 b' . unHash $ countCounts freqs
    where
        b' :: Sum Int
        b' = foldMap (Sum . (\a -> a - bins freqs)) b

-- | Returns the cumulative frequencies for the sample given in the
-- @Traversable@.
cumulativeFreqs :: (Eq a, Hashable a, Traversable t)
                => FreqMap a -> t a -> t (a, Double)
cumulativeFreqs freqs = snd . mapAccumL step 0.0
    where
        step c x = let c' = c + fromIntegral (lookup x freqs)
                   in  (c', (x, c'))

-- Returns the frequency from the FreqMap or 0.
lookup :: (Eq a, Hashable a) => a -> FreqMap a -> Int
lookup x = getSum . M.lookupDefault mempty x . unHash

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm'@(MHash fm) = fromIntegral (grandTotal fm')
                              / fromIntegral (M.size fm)

-- | Returns the value with the greatest frequency.
maxValue :: FreqMap a -> Maybe a
maxValue (MHash fm)
    | M.size fm == 0 = Nothing
    | otherwise      = Just . fst . L.maximumBy (comparing snd) $ M.toList fm

-- | Take a freq map and return a freq map of the frequencies.
countCounts :: FreqMap a -> FreqMap Int
countCounts = frequencies . fmap getSum . M.elems . unHash

-- | Converts a frequency mapping to a mapping of probabilities.
probabilities :: FreqMap a -> M.HashMap a Double
probabilities (MHash fq) =
    (/ n) . fromIntegral . getSum <$> fq
    where
        n = fromIntegral . getSum . fold $ M.elems fq

lookupCond :: (Eq a, Hashable a, Eq b, Hashable b)
           => ConditionalFreq a b -> a -> b -> Int
lookupCond (MHash cm) a b = getSum . fold $ M.lookup b . unHash =<< M.lookup a cm
