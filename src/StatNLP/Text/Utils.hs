

module StatNLP.Text.Utils where


import           Conduit
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Traversable

import           StatNLP.Types


count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = M.insertWith (+) a 1 m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count M.empty

frequenciesC :: (Monad m, Eq a, Hashable a) => Consumer a m (FreqMap a)
frequenciesC = foldlC count M.empty

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm = fromIntegral (sum (M.elems fm))
                  / fromIntegral (M.size fm)

ngrams :: Int -> [a] -> [[a]]
ngrams n = filter ((== n) . length) . map (take n) . L.tails

ngramsC :: Monad m => Int -> Conduit a m [a]
ngramsC n = slidingWindowC n
