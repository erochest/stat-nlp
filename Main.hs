{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Arrow       ((&&&))
import           Control.DeepSeq
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

dataFile :: FilePath
dataFile = "/Users/err8n/s/stat-nlp/data/austen-cntcnt.txt"

main :: IO ()
main = do
    counts <-  force . MHash . fmap Sum
           <$> (readHashMap dataFile :: IO (M.HashMap Int Int))
    let -- c1 = force . M.toList . uncurry (flip (M.insert 0)) $ sgt  counts 1.96
        c1 = []
        c2 = force
           . M.toList
           . uncurry (flip (M.insert 0))
           . fmap (0,)
           $ sgt counts 1.96
    mapM_ (F.print "{}\t{}\t{}\n" . flatten) . L.sortBy (comparing fst) $ c1 ++ c2

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


flatten :: (a, (b, c)) -> (a, b, c)
flatten (a, (b, c)) = (a, b, c)
