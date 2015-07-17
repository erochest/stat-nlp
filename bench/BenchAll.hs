
module Main where


import           Control.DeepSeq
import           Criterion.Main
import qualified Data.HashMap.Strict as M
import           Data.Monoid

import           StatNLP.Statistics
import           StatNLP.Types
import           StatNLP.Utils


dataFile :: FilePath
dataFile = "/Users/err8n/s/stat-nlp/data/austen-cntcnt.txt"

main :: IO ()
main = do
    counts <-  force . MHash . fmap Sum
           <$> (readHashMap dataFile :: IO (M.HashMap Int Int))
    defaultMain [ bgroup "sgt" [ bench "sgt"  $ nf (sgt  counts) 1.96
                               , bench "sgt'" $ nf (sgt' counts) 1.96
                               ]
                ]
