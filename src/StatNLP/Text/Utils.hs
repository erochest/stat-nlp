

module StatNLP.Text.Utils where


import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector         as V

import           StatNLP.Types


count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = MHash . M.insertWith mappend a 1 $ unHash m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count mempty

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio (MHash fm) = fromIntegral (getSum . sum $ M.elems fm)
                          / fromIntegral (M.size fm)

ngrams :: Int -> [a] -> [[a]]
ngrams n = filter ((== n) . length) . map (take n) . L.tails

ngramsV :: Int -> V.Vector a -> [V.Vector a]
ngramsV n vs = map (\i -> V.slice i n vs) [0..(V.length vs - n - 1)]
