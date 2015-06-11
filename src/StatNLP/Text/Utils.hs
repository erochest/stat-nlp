{-# LANGUAGE RankNTypes #-}


module StatNLP.Text.Utils where


import           Conduit
import           Control.Arrow
import           Control.Monad.Par
import           Control.Monad.Par.Combinator
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict          as M
import qualified Data.List                    as L
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector                  as V

import           StatNLP.Statistics
import           StatNLP.Types


count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = MHash . M.insertWith mappend a 1 $ unHash m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count mempty

frequenciesC :: (Eq a, Hashable a, Monad m)
             => Consumer a m (FreqMap a)
frequenciesC = foldlC count mempty

grandTotal :: FreqMap a -> Int
grandTotal = getSum . fold . M.elems . unHash

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm'@(MHash fm) = fromIntegral (grandTotal fm')
                              / fromIntegral (M.size fm)

ngrams :: Int -> [a] -> [[a]]
ngrams n = filter ((== n) . length) . map (take n) . L.tails

ngramsV :: Int -> V.Vector a -> [V.Vector a]
ngramsV n vs = map (\i -> V.slice i n vs) [0..(V.length vs - n - 1)]

ngramsC :: Monad m => Int -> Conduit a m (V.Vector a)
ngramsC = slidingWindowC

instance Hashable a => Hashable (V.Vector a) where
    hashWithSalt x = hashWithSalt x . V.toList
    hash = hash . V.toList

tTestNGramFreqs :: (Eq a, Hashable a)
                => FreqMap (V.Vector a) -- ^ frequencies of n-grams
                -> FreqMap a            -- ^ frequencies of items
                -> V.Vector a           -- ^ target n-gram
                -> Double               -- ^ t-test output
tTestNGramFreqs ngrams items =
    tTestNGramFreqsTotals ngrams (grandTotal ngrams) items (grandTotal items)

tTestNGramFreqsTotals :: (Eq a, Hashable a)
                      => FreqMap (V.Vector a)   -- ^ frequencies of n-grams
                      -> Int                    -- ^ total number of n-grams
                      -> FreqMap a              -- ^ frequencies of items
                      -> Int                    -- ^ total number of items
                      -> V.Vector a             -- ^ target n-gram
                      -> Double                 -- ^ t-test output
tTestNGramFreqsTotals (MHash ngrams) totalNGrams (MHash items) totalItems target =
    tTest expected actual stddev totalNGrams
    where
        lookup hash x = getSum $ M.lookupDefault 0 x hash
        expected = getProduct
                 $ foldMap ( Product
                           . (/ totalItems')
                           . fromIntegral
                           . lookup items)
                   target

        actualFreq = fromIntegral $ lookup ngrams target
        actual     = actualFreq / totalNGrams'

        totalNGrams' :: Double
        totalNGrams' = fromIntegral totalNGrams

        totalItems' :: Double
        totalItems' = fromIntegral totalItems

        stddev = sqrt $ (1.0 / totalNGrams') * (hits + misses)
        hits   = actualFreq * (1.0 - actual)^2
        misses = (totalNGrams' - actualFreq) * (0.0 - actual)^2

tTestNGramMatrix :: (Eq a, Hashable a, NFData a)
                 => FreqMap (V.Vector a)
                 -> FreqMap a
                 -> M.HashMap (V.Vector a) Double
tTestNGramMatrix ngrams = M.fromList . tTestNGramMatrixList ngrams

tTestNGramMatrixList :: (Eq a, Hashable a, NFData a)
                     => FreqMap (V.Vector a)
                     -> FreqMap a
                     -> [(V.Vector a, Double)]
tTestNGramMatrixList ngrams items =
      runPar
    . parMap (id &&& tTestNGramFreqsTotals ngrams ngramsTotal items itemsTotal)
    . M.keys
    $ unHash ngrams
    where
        ngramsTotal = grandTotal ngrams
        itemsTotal  = grandTotal items
