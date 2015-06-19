{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Freqs where


import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
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

replaceFreqsFromset :: (Eq a, Hashable a)
                    => S.HashSet a -> a -> FreqMap a -> FreqMap a
replaceFreqsFromset set with = MHash
                             . M.fromListWith mappend
                             . fmap repl
                             . M.toList
                             . unHash
    where
        repl p@(a, c) | a `S.member` set = (with, c)
                      | otherwise        = p
