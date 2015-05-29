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
import           StatNLP.Output
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
    (corpusPath, mtarget) <- parseArgs

    -- stopwords
    stopwords <-  S.fromList . fmap _tokenNorm . tokenizer
              <$> TIO.readFile "corpora/stopwords/english"

    F.print "Reading corpus from {}\n" $ F.Only corpusPath
    corpus <- loadCorpusDirectory tokenizer reader return corpusPath
    docs   <- readCorpusVectors mtarget corpus
    F.print "Documents potentially containing '{}' : {} / {}\n"
            (fromMaybe "0" mtarget, L.length docs, M.size (_corpusDocuments corpus))

    let InverseIndex index  = foldMap inverseIndexDocument docs
        targets             = L.sort $ maybe (M.keys index) return mtarget

    mapM_ (const (putStrLn "") <=< printColls index docs) targets
