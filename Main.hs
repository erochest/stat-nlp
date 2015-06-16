{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Conduit
import           Control.Arrow           ((&&&))
import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.BloomFilter.Easy
import qualified Data.BloomFilter.Easy   as BF
import qualified Data.ByteString         as B
import           Data.Conduit.List       (sourceList)
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeLatin1)
import qualified Data.Text.Format        as F
import qualified Data.Text.IO            as TIO
import           Data.Text.Lazy          (toStrict)
import           Data.Text.Lazy.Builder  (toLazyText)
import           Data.Time
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector             as V
import           GHC.Exts                hiding (toList)

import           Debug.Trace

import           StatNLP.Corpus
import           StatNLP.Document
import           StatNLP.Input
import           StatNLP.Output          hiding (flatten)
import           StatNLP.Output.Kwic
import           StatNLP.Text.Collocates
import           StatNLP.Text.Index      hiding (toList)
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


main :: IO ()
main = do
    corpusPath <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . fmap _tokenNorm . tokenizer
              <$> TIO.readFile "corpora/stopwords/english"

    F.print "Reading corpus from {}\n" $ F.Only corpusPath
    corpus <- loadCorpusDirectory (tokenizerStop stopwords) reader return corpusPath
    docs   <- readCorpusVectors Nothing corpus
    F.print "Documents read {}\n" . F.Only . M.size $ _corpusDocuments corpus

    let tokens = fmap (fmap _tokenNorm . _documentTokens) $ M.elems docs
        freqs  = foldParMapChunk 1024 frequencies tokens
        ngrams = foldParMapChunk 1024 (frequencies . mapMaybe vectorPair . ngramsV 2)
                                 tokens
    mapM_ (F.print "{} ({})\t{} ({})\t{}\t{}\n")
        . fmap flatten
        . L.sortBy (comparing (Down . third))
        $ pointwiseMIMatrixList freqs ngrams

flatten :: (((a, b), (c, d)), e, f) -> (a, b, c, d, e, f)
flatten (((a, b), (c, d)), e, f) = (a, b, c, d, e, f)
