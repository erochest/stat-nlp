{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output where


import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Monoid
import           Data.Ord
import           Data.Text.Buildable
import qualified Data.Text.Format    as F

import           StatNLP.Types


freqReport :: Buildable ps => Int -> FreqMap ps -> IO ()
freqReport n = mapM_ (F.print "{}\t{}\n")
             . fmap (fmap getSum)
             . L.take n
             . L.sortBy (comparing (Down . snd))
             . M.toList
             . unHash

