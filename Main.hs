{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.DeepSeq
import           Control.Monad.Identity
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import qualified Data.List                 as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeLatin1)
import qualified Data.Text.Format          as F
import qualified Data.Text.IO              as TIO
import           Data.Text.Lazy            (toStrict)
import           Data.Text.Lazy.Builder    (toLazyText)
import           Data.Time
import           Data.Traversable
import qualified Data.Vector               as V
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude                   hiding (FilePath, readFile)
import           Taygeta.Tokenizer         (regexTokenizer)

import           StatNLP.Corpus
import           StatNLP.Document
import           StatNLP.Output
import           StatNLP.Output.Kwic
import           StatNLP.Text.Collocates
import           StatNLP.Text.Index
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
    stopwords <-  S.fromList . map (_tokenNorm . normalize) . tokenize
              <$> TIO.readFile "corpora/stopwords/english"

    let reader    = fmap decodeLatin1 . readFile . _documentId
        tokenizer = filter (not . (`S.member` stopwords) . _tokenNorm)
                  . fmap normalize
                  . tokenize

    corpus <- loadCorpusDirectory tokenizer reader corpusPath
    let docs = L.sortBy (comparing _documentId) . M.elems $ _corpusDocuments corpus

    colls <-  M.toList . unHash . fold
          <$> mapM ( fmap (frequencies . collocates 0 2 . fmap _tokenNorm . _documentTokens)
                   . tokenizeDocument corpus) docs

    let filterf a ((b, _), _) = a == b
    mapM_ (F.print "{} {}\t{}\n")
        . fmap (\((a, b), c) -> (a, b, c))
        . take 25
        . L.sortBy (comparing (Down . snd))
        . fmap (fmap getSum)
        $ maybe colls ((`filter` colls) . filterf) mtarget

    putStrLn "done!"


freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show

time :: NFData a => IO a -> IO a
time m = do
    start <- getCurrentTime
    a     <- m
    end   <- a `deepseq` getCurrentTime
    F.print "Elapsed time: {}\n\n" . F.Only . F.Shown $ end `diffUTCTime` start
    return a
