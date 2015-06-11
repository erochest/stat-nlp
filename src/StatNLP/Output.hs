{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output where


import           Conduit
import           Control.Lens
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Data.Text.Buildable
import qualified Data.Text.Format        as F
import qualified Data.Vector             as V

import           StatNLP.Document
import           StatNLP.Statistics
import           StatNLP.Text.Collocates
import           StatNLP.Text.Utils
import           StatNLP.Types


freqReport :: Buildable ps => Int -> FreqMap ps -> IO ()
freqReport n = mapM_ (F.print "{}\t{}\n")
             . fmap (fmap getSum)
             . L.take n
             . L.sortBy (comparing (Down . snd))
             . M.toList
             . unHash

flatten :: ((a, b), SummaryStats) -> (a, b, Int, Double, Double)
flatten ((a, b), (SummaryStats c d e)) = (a, b, c, d, e)

printColls :: M.HashMap PlainToken [(DocumentId, Int)]
           -> M.HashMap DocumentId (VectorDoc b)
           -> T.Text
           -> IO ()
printColls index docs target = do
    stats <- fmap collocateStatsCFin
          .  runResourceT $  getDocsC index docs target
                          =$ getCollocatesC 0 3
                          $$ collocateStatsC
    mapM_ (F.print "{}\t{}\t{}\t{}\t{}\n")
        $ prepCollocates stats

prepCollocates :: M.HashMap (a, a) (Maybe SummaryStats) -> [(a, a, Int, Double, Double)]
prepCollocates = fmap flatten
               . L.sortBy (comparing (^. _2 . summaryVariance))
               . mapMaybe sequenceA
               . M.toList

freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show
