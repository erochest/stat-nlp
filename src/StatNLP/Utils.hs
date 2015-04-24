module StatNLP.Utils where


import           Conduit
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M

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

collocates :: Traversable t => Int -> Int -> t a -> t (a, a)
collocates = undefined

