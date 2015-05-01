{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.DeepSeq
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeLatin1)
import qualified Data.Text.Format          as F
import qualified Data.Text.IO              as TIO
import           Data.Time
import           Data.Traversable
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude                   hiding (FilePath, readFile)
import           Taygeta.Tokenizer         (regexTokenizer)

import           StatNLP.Corpus
import           StatNLP.Output
import           StatNLP.Text.Collocates
import           StatNLP.Text.Tokens
import           StatNLP.Text.Utils
import           StatNLP.Types
import           StatNLP.Utils

import           Opts

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: concordance/kwic generator
-}


main :: IO ()
main = do
    corpusPath <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . map (T.toLower . tokenNorm) . tokenize
              <$> TIO.readFile "corpora/stopwords/english"

    let reader    = fmap (T.lines . decodeLatin1) . readFile . documentId
        tokenizer = filter (not . (`S.member` stopwords) . tokenNorm)
                  . fmap normalize
                  . tokenize

    corpus <- loadCorpusDirectory tokenizer reader corpusPath
    let docs = M.elems $ corpusDocuments corpus
    tokens <- time $ do
        putStrLn "Tokenizing"
        map tokenNorm . concat <$> mapM (documentTokens corpus) docs

    -- frequencies
    putStrLn "Frequencies"
    time $ do
        let freqs = frequencies tokens
        freqReport 25 freqs
        F.print "Token/type ratio = {}\n\n" . F.Only $ tokenTypeRatio freqs

    -- collocates
    time $ do
        putStrLn "Collocates"
        freqShowReport 25 $ collocates 3 3 tokens

    -- n-grams
    time $ do
        putStrLn "\nBigrams"
        freqShowReport 25 $ ngrams 2 tokens
    time $ do
        putStrLn "\nTrigrams"
        freqShowReport 25 $ ngrams 3 tokens

freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show

time :: NFData a => IO a -> IO a
time m = do
    start <- getCurrentTime
    a     <- m
    end   <- a `deepseq` getCurrentTime
    F.print "Elapsed time: {}\n\n" . F.Only . F.Shown $ end `diffUTCTime` start
    return a
