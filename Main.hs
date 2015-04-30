{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Hashable
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import qualified Data.Text               as T
import qualified Data.Text.Format        as F
import qualified Data.Text.IO            as TIO
import           Data.Traversable
import           Taygeta.Tokenizer       (regexTokenizer)

import           StatNLP.Corpus
import           StatNLP.Output
import           StatNLP.Text.Collocates
import           StatNLP.Text.Utils
import           StatNLP.Types

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: concordance/kwic generator
-}

tokenize :: PlainTokenizer
tokenize = regexTokenizer "[\\p{L}\\p{M}]+"

main :: IO ()
main = do
    -- stopwords
    stopwords <-  S.fromList . map T.toLower . tokenize
              <$> TIO.readFile "corpora/stopwords/english"

    -- tokens
    {-
     - tokens <- runResourceT $  stdinC
     -                        $= CT.linesBounded (2^30)
     -                        $= concatMapC tokenize
     -                        $= mapC T.toLower
     -                        $= filterC (not . (`S.member` stopwords))
     -                        $$ sinkList
     -}
    let tokens = [] :: [PlainToken]

    -- frequencies
    let freqs = frequencies tokens
    putStrLn "Frequencies"
    freqReport 25 freqs
    F.print "Token/type ratio = {}\n\n" . F.Only $ tokenTypeRatio freqs

    -- collocates
    putStrLn "Collocates"
    freqShowReport 25 $ collocates 3 3 tokens

    -- n-grams
    putStrLn "\nBigrams"
    freqShowReport 25 $ ngrams 2 tokens
    putStrLn "\nTrigrams"
    freqShowReport 25 $ ngrams 3 tokens

freqShowReport :: (Eq a, Hashable a, Show a) => Int -> [a] -> IO ()
freqShowReport n = freqReport n
                 . M.fromList
                 . fmap (first show)
                 . M.toList
                 . frequencies
