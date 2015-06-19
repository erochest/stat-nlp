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
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Data.Tuple
import           Data.Vector                  ((!?))
import qualified Data.Vector                  as V

import           StatNLP.Statistics
import           StatNLP.Types
import           StatNLP.Utils


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

bigrams :: [a] -> [(a, a)]
bigrams xs = [(a, b) | [a, b] <- ngrams 2 xs]

trigrams :: [a] -> [(a, a, a)]
trigrams xs = [(a, b, c) | [a, b, c] <- ngrams 3 xs]

fourgrams :: [a] -> [(a, a, a, a)]
fourgrams xs = [(a, b, c, d) | [a, b, c, d] <- ngrams 4 xs]

ngramsV :: Int -> V.Vector a -> [V.Vector a]
ngramsV n vs = map (\i -> V.slice i n vs) [0..(V.length vs - n - 1)]

bigramsV :: V.Vector a -> [(a, a)]
bigramsV = mapMaybe tuple . ngramsV 2
    where
        tuple v = (,) <$> v !? 0 <*> v !? 1

trigramsV :: V.Vector a -> [(a, a, a)]
trigramsV = mapMaybe tuple . ngramsV 3
    where
        tuple v = (,,) <$> v !? 0 <*> v !? 1 <*> v !? 2

fourgramsV :: V.Vector a -> [(a, a, a, a)]
fourgramsV = mapMaybe tuple . ngramsV 4
    where
        tuple v = (,,,) <$> v !? 0 <*> v !? 1 <*> v !? 2 <*> v !? 3

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

tDifferenceMatrixList :: (Eq a, Hashable a, NFData a, Ord a)
                      => FreqMap (a, a)
                      -> a
                      -> [((a, Int), (a, Int), Double)]
tDifferenceMatrixList freqs a =
    runPar $ parMap (uncurry tDiff) pairs
    where
        tDiff a@(_, fa) b@(_, fb) = (a, b, tTestDifferences fa fb)
        counts = fmap (fmap getSum)
               . M.toList
               . M.fromListWith mappend
               . fmap swap
               . mapMaybe (sequenceA . fmap (collocate a) . swap)
               . filter ((> 1) . snd)
               . M.toList
               $ unHash freqs
        pairs = [ordPair v1 v2 | v1 <- counts, v2 <- counts, fst v1 /= fst v2]

        ordPair a b | a <= b    = (a, b)
                    | otherwise = (b, a)

        collocate a (b, c) | a == b    = Just c
                           | a == c    = Just b
                           | otherwise = Nothing

lookup' :: (Eq a, Hashable a) => M.HashMap a (Sum Int) -> a -> Int
lookup' m k = getSum $ M.lookupDefault mempty k m

likelihoodMatrixList :: (Eq a, Hashable a, NFData a)
                     => FreqMap a
                     -> FreqMap (a, a)
                     -> [(((a, Double), (a, Double)), Double, Double)]
likelihoodMatrixList freqs ngrams =
    filter (isNum . third) $ matrixList lhr ngrams
    where
        isNum x     = not $ isNaN x || isInfinite x
        totalFreqs' = grandTotal freqs
        totalFreqs  = fromIntegral totalFreqs'
        totalNGrams = fromIntegral $ grandTotal ngrams
        freqs'      = unHash freqs
        lhr ng@(a, b) c = let ac = lookup' freqs' a
                              bc = lookup' freqs' b
                              ap = (fromIntegral ac) / totalFreqs
                              bp = (fromIntegral bc) / totalFreqs
                              lr = likelihoodRatio ap ac bp bc
                                           (fromIntegral c / totalNGrams) c
                                           totalFreqs'
                          in  (((a, ap), (b, bp)), fromIntegral c / totalNGrams, lr)

pointwiseMIMatrixList :: (Eq a, Hashable a, NFData a)
                      => FreqMap a
                      -> FreqMap (a, a)
                      -> [(((a, Double), (a, Double)), Double, Double)]
pointwiseMIMatrixList freqs ngrams = matrixList mi ngrams
    where
        mi ng@(a, b) c =
            let a' = fromIntegral (lookup' freqs' a) / total
                b' = fromIntegral (lookup' freqs' b) / total
            in  ( ((a, a'), (b, b'))
                , fromIntegral c / ngTotal
                , pointwiseMI a' b'
                )

        freqs'  = unHash freqs
        total   = fromIntegral $ grandTotal freqs
        ngTotal = fromIntegral $ grandTotal ngrams

mleMatrixList :: (Eq a, Hashable a, NFData a)
              => FreqMap a
              -> FreqMap (a, a, a)
              -> [(a, a, a, Double)]
mleMatrixList freqs g3 =
    runPar . fmap concat
           . parMap mlePair
           . M.toList
           $ unHash g2
    where
        dropThird (a, b, _) = (a, b)
        g2 = frequencyBy dropThird third . M.keys $ unHash g3
        mlePair ((a, b), freqs) =
            map (uncurry (mle' a b total) . fmap getSum)
                . M.toList
                $ unHash freqs
            where
                total = grandTotal freqs
        mle' a b cn1 c cn = (a, b, c, mle cn cn1)

matrixList :: (NFData b) => (a -> Int -> b) -> FreqMap a -> [b]
matrixList f freqs =
    runPar . parMap (uncurry f . fmap getSum)
           . M.toList
           $ unHash freqs

vectorPair :: V.Vector a -> Maybe (a, a)
vectorPair v | V.length v >= 2 = Just (v V.! 0, v V.! 1)
             | otherwise       = Nothing
