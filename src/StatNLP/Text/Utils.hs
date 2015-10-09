{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module StatNLP.Text.Utils where


import           Conduit
import           Control.Arrow
import           Control.Monad.Par
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tuple
import           Data.Vector         ((!?))
import qualified Data.Vector         as V

import           StatNLP.Statistics
import           StatNLP.Text.Freqs
import           StatNLP.Types
import           StatNLP.Utils


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
tTestNGramFreqs ngrms items =
    tTestNGramFreqsTotals ngrms (grandTotal ngrms) items (grandTotal items)

tTestNGramFreqsTotals :: (Eq a, Hashable a)
                      => FreqMap (V.Vector a)   -- ^ frequencies of n-grams
                      -> Int                    -- ^ total number of n-grams
                      -> FreqMap a              -- ^ frequencies of items
                      -> Int                    -- ^ total number of items
                      -> V.Vector a             -- ^ target n-gram
                      -> Double                 -- ^ t-test output
tTestNGramFreqsTotals (MHash ngrms) totalNGrams (MHash items) totalItems target =
    tTest expected actual stddev totalNGrams
    where
        lu h x   = getSum $ M.lookupDefault 0 x h
        expected = getProduct
                 $ foldMap ( Product
                           . (/ totalItems')
                           . fromIntegral
                           . lu items)
                   target

        actualFreq = fromIntegral $ lu ngrms target
        actual     = actualFreq / totalNGrams'

        totalNGrams' :: Double
        totalNGrams' = fromIntegral totalNGrams

        totalItems' :: Double
        totalItems' = fromIntegral totalItems

        stddev = sqrt $ (1.0 / totalNGrams') * (hits + misses)
        hits   = actualFreq * (1.0 - actual)^(2 :: Int)
        misses = (totalNGrams' - actualFreq) * (0.0 - actual)^(2 :: Int)

tTestNGramMatrix :: (Eq a, Hashable a, NFData a)
                 => FreqMap (V.Vector a)
                 -> FreqMap a
                 -> M.HashMap (V.Vector a) Double
tTestNGramMatrix ngrms = M.fromList . tTestNGramMatrixList ngrms

tTestNGramMatrixList :: (Eq a, Hashable a, NFData a)
                     => FreqMap (V.Vector a)
                     -> FreqMap a
                     -> [(V.Vector a, Double)]
tTestNGramMatrixList ngrms items =
      runPar
    . parMap (id &&& tTestNGramFreqsTotals ngrms ngramsTotal items itemsTotal)
    . M.keys
    $ unHash ngrms
    where
        ngramsTotal = grandTotal ngrms
        itemsTotal  = grandTotal items

tDifferenceMatrixList :: (Eq a, Hashable a, NFData a, Ord a)
                      => FreqMap (a, a)
                      -> a
                      -> [((a, Int), (a, Int), Double)]
tDifferenceMatrixList freqs item =
    runPar $ parMap (uncurry tDiff) pairs
    where
        tDiff a@(_, fa) b@(_, fb) = (a, b, tTestDifferences fa fb)
        counts = fmap (fmap getSum)
               . M.toList
               . M.fromListWith mappend
               . fmap swap
               . mapMaybe (sequenceA . fmap (collocate item) . swap)
               . filter ((> 1) . snd)
               . M.toList
               $ unHash freqs
        pairs = [ordPair v1 v2 | v1 <- counts, v2 <- counts, fst v1 /= fst v2]

        ordPair a b | a <= b    = (a, b)
                    | otherwise = (b, a)

        collocate a (b, c) | a == b    = Just c
                           | a == c    = Just b
                           | otherwise = Nothing

likelihoodMatrixList :: (Eq a, Hashable a, NFData a)
                     => FreqMap a
                     -> FreqMap (a, a)
                     -> [(((a, Double), (a, Double)), Double, Double)]
likelihoodMatrixList freqs ngrms =
    filter (isNum . third) $ matrixList lhr ngrms
    where
        isNum x     = not $ isNaN x || isInfinite x
        totalFreqs' = grandTotal freqs
        totalFreqs  = fromIntegral totalFreqs'
        totalNGrams = fromIntegral $ grandTotal ngrms
        freqs'      = unHash freqs
        lhr (a, b) c = let ac = lookup' freqs' a
                           bc = lookup' freqs' b
                           ap = fromIntegral ac / totalFreqs
                           bp = fromIntegral bc / totalFreqs
                           lr = likelihoodRatio ap ac bp bc
                                        (fromIntegral c / totalNGrams) c
                                        totalFreqs'
                       in  (((a, ap), (b, bp)), fromIntegral c / totalNGrams, lr)

pointwiseMIMatrixList :: (Eq a, Hashable a, NFData a)
                      => FreqMap a
                      -> FreqMap (a, a)
                      -> [(((a, Double), (a, Double)), Double, Double)]
pointwiseMIMatrixList freqs ngrms = matrixList mi ngrms
    where
        mi (a, b) c =
            let a' = fromIntegral (lookup' freqs' a) / total'
                b' = fromIntegral (lookup' freqs' b) / total'
            in  ( ((a, a'), (b, b'))
                , fromIntegral c / ngTotal
                , pointwiseMI a' b'
                )

        freqs'  = unHash freqs
        total'  = fromIntegral $ grandTotal freqs
        ngTotal = fromIntegral $ grandTotal ngrms

mleMatrixList :: (Ord a, Eq a, Hashable a, NFData a)
              => FreqMap a
              -> FreqMap (a, a)
              -> [(a, a, Int, Double)]
mleMatrixList _ g3 =
    runPar . fmap (concat . catMaybes)
           . parMap mlePair
           . M.toList
           $ unHash g2
    where
        g2 = groupFreqsBy fst snd g3
        mlePair (a, fqs@(MHash fqs'))
            | M.size fqs' > 1 = Just . map ( uncurry (mle' a total')
                                           . fmap getSum)
                                     $ M.toList fqs'
            | otherwise = Nothing
            where
                total' = grandTotal fqs
        mle' a cn1 b cn = (a, b, cn, mle cn cn1)

matrixList :: (NFData b) => (a -> Int -> b) -> FreqMap a -> [b]
matrixList f freqs =
    runPar . parMap (uncurry f . fmap getSum)
           . M.toList
           $ unHash freqs

vectorPair :: V.Vector a -> Maybe (a, a)
vectorPair v | V.length v >= 2 = Just (v V.! 0, v V.! 1)
             | otherwise       = Nothing

type FreqState a = (FreqMap a, ConditionalFreq a a, ConditionalFreq (a, a) a)

-- | This builds unigram, bigram, and trigram models from a single input
-- stream. It should be relatively efficient.
nGramModels :: (Hashable a, Eq a)
            => [a]
            -> (FreqMap a, ConditionalFreq a a, ConditionalFreq (a, a) a)
nGramModels = L.foldl' step (mempty, mempty, mempty)
            . map (L.take 3 . L.tail . L.inits)
            . L.tails
    where
        step :: (Eq x, Hashable x) => FreqState x -> [[x]] -> FreqState x
        step (uni, bi, tri) [[u], [b0, b1], [t0, t1, t2]] =
            (count uni u, count bi (b0, b1), count tri ((t0, t1), t2))
        step freqs _ = freqs

-- | This performs deleted interpolation. If you use currying to store the
-- function returned after the list of items, it caches the n-gram models.
ngramInterpolation :: (Hashable a, Eq a)
                   => (Double, Double, Double)
                   -- ^ Weightings. These can be determined manually, but
                   -- it's probably better to use expectation
                   -- maximization.
                   -> [a]
                   -- ^ The input list of items.
                   -> (a, a, a)
                   -- ^ The items to determine the probability for. The
                   -- items in the tuple are in order @(W1, W2, W3)@.
                   -> Double
ngramInterpolation (l1, l2, l3) xs =
    let (p1, p2, p3) = probs $ nGramModels xs
    in  \(w1, w2, w3) -> l1 * M.lookupDefault 0.0 w3 p1
                      +  l2 * lookupDefault p2 w2 w3
                      +  l3 * lookupDefault p3 (w1, w2) w3
    where
        probs (u, MHash b, MHash t) =
            (probabilities u, probabilities <$> b, probabilities <$> t)
        lookupDefault m k0 k1 = fromMaybe 0.0 $ M.lookup k1 =<< M.lookup k0 m
