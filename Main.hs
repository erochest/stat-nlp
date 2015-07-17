{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Arrow       ((&&&))
import           Control.Monad       (forM_)
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text           as T
import qualified Data.Text.Format    as F
import qualified Data.Text.IO        as TIO

import           Debug.Trace

import           StatNLP.Corpus
import           StatNLP.Input
import           StatNLP.Statistics
import           StatNLP.Text.Freqs
import           StatNLP.Text.Tokens
import           StatNLP.Text.Utils
import           StatNLP.Types
import           StatNLP.Utils

import           Opts

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: probability distributions
 -}


-- TODO: append this to the beginning of each document
start :: T.Text
start = "*"

unknown :: T.Text
unknown = "<UNK>"

main :: IO ()
main = do
    corpusPath <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . fmap _tokenNorm . tokenizer
              <$> TIO.readFile "corpora/stopwords/english"

    F.print "Reading corpus from {}\n" $ F.Only corpusPath
    corpus <- loadCorpusDirectory tokenizer reader return corpusPath
    docs   <- readCorpusVectors Nothing corpus
    F.print "Documents read {}\n" . F.Only . M.size $ _corpusDocuments corpus

    let tokens' = fmap _tokenNorm . _documentTokens <$> M.elems docs
        freqs'  = foldParMap frequencies tokens'
        hapax   = hapaxLegomena freqs'
        tokens  = fmap (replaceFromSet hapax unknown) tokens'
        freqs   = replaceFreqsFromSet hapax unknown freqs'
        ngrams  = foldParMap (frequencies . bigramsV) tokens

    putStrLn "r\tr*\tPgt\n"
    -- mapM_ (F.print "{}\t{}\t{}\n") $ sgt ngrams

tokenizer' :: StopWords -> Tokenizer (Token LinePos PlainToken)
tokenizer' = tokenizerStop
{-
 - tokenizer' stopwords input =
 -     runIdentity $  sourceList (T.lines input)
 -                 $= tokenStopC stopwords
 -                 $$ sinkList
 -}

sortOn :: (a, b, c, d) -> (a, Down c, b)
sortOn (a, b, c, _) = (a, Down c, b)


flatten :: (((a, b), (c, d)), e, f) -> (a, b, c, d, e, f)
flatten (((a, b), (c, d)), e, f) = (a, b, c, d, e, f)
