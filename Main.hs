{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Conduit
import qualified Data.Conduit.Text   as CT
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Ord
import qualified Data.Text           as T
import qualified Data.Text.Format    as F
import qualified Data.Text.IO        as TIO
import           Taygeta.Tokenizer   (regexTokenizer)
import           Taygeta.Types

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: collocate generator
 - TODO: n-gram generator
 - TODO: stopword filter
 - TODO: inverse index
 - TODO: concordance/kwic generator
 - TODO: kwic frequency tree
-}


type FreqMap a = M.HashMap a Int

count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = M.insertWith (+) a 1 m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count M.empty

frequenciesC :: (Monad m, Eq a, Hashable a) => Consumer a m (FreqMap a)
frequenciesC = foldlC count M.empty

freqReport :: Int -> FreqMap PlainToken -> IO ()
freqReport n = mapM_ (F.print "{}\t{}\n")
             . L.take n
             . L.sortBy (comparing (Down . snd))
             . M.toList

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm = fromIntegral (sum (M.elems fm))
                  / fromIntegral (M.size fm)

main :: IO ()
main = do
    freqs <- runResourceT $  stdinC
                          $= CT.linesBounded (2^30)
                          $= concatMapC (regexTokenizer "[\\p{L}\\p{M}]+")
                          $= mapC T.toLower
                          $$ frequenciesC
    freqReport 25 freqs
    F.print "Token/type ratio = {}\n" . F.Only $ tokenTypeRatio freqs
