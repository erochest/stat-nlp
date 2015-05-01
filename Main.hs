{-# LANGUAGE OverloadedStrings #-}


module Main where


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
import           Data.Traversable
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS
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

    let reader    = fmap decodeLatin1 . readFile
        tokenizer = filter (not . (`S.member` stopwords) . tokenNorm)
                  . fmap normalize
                  . tokenize
    corpus <- loadCorpusDirectory reader tokenizer corpusPath

    -- frequencies
    let freqs = frequencies
              . fmap tokenNorm
              . mconcat
              . map documentTokens
              . M.elems
              $ corpusDocuments corpus
    putStrLn "Frequencies"
    freqReport 25 freqs
    F.print "Token/type ratio = {}\n\n" . F.Only $ tokenTypeRatio freqs

    -- collocates
{-
 -     putStrLn "Collocates"
 -     freqShowReport 25 $ collocates 3 3 tokens
 -
 -     -- n-grams
 -     let tokenList = V.toList tokens
 -     putStrLn "\nBigrams"
 -     freqShowReport 25 $ ngrams 2 tokenList
 -     putStrLn "\nTrigrams"
 -     freqShowReport 25 $ ngrams 3 tokenList
 -}

freqShowReport :: (Traversable t, Eq a, Hashable a, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n
                 . M.fromList
                 . fmap (first show)
                 . M.toList
                 . frequencies
