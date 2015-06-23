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
import qualified Data.Text           as T
import           Data.Traversable

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

count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = MHash . M.insertWith mappend a 1 $ unHash m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count mempty

frequencyBy :: (Eq k, Hashable k, Eq v, Hashable v, Foldable f)
            => (a -> k) -> (a -> v) -> f a -> MonoidHash k (FreqMap v)
frequencyBy by value =
    foldMap ( MHash
            . uncurry M.singleton
            . (by &&& (MHash . (`M.singleton` 1) . value)))

groupFreqsBy :: (Ord k, Eq k, Hashable k, Eq v, Hashable v)
             => (a -> k) -> (a -> v) -> FreqMap a -> MonoidHash k (FreqMap v)
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

frequenciesC :: (Eq a, Hashable a, Monad m)
             => Consumer a m (FreqMap a)
frequenciesC = foldlC count mempty

grandTotal :: FreqMap a -> Int
grandTotal = getSum . fold . M.elems . unHash

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm'@(MHash fm) = fromIntegral (grandTotal fm')
                              / fromIntegral (M.size fm)
