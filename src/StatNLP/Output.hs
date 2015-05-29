{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output where


import           Conduit
import           Control.Lens
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text               as T
import           Data.Text.Buildable
import qualified Data.Text.Format        as F

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

flatten :: ((a, b), (c, d, e)) -> (a, b, c, d, e)
flatten ((a, b), (c, d, e)) = (a, b, c, d, e)

printColls :: M.HashMap PlainToken [(DocumentId, Int)]
           -> M.HashMap DocumentId (VectorDoc b)
           -> T.Text
           -> IO ()
printColls index docs target = do
    stats <- fmap collocateStatsCFin
          .  runResourceT $  getDocsC index docs target
                          =$ getCollocatesC
                          $$ collocateStatsC
    mapM_ (F.print "{}\t{}\t{}\t{}\t{}\n")
        $ prepCollocates stats

prepCollocates :: M.HashMap (a, a) (Int, Double, Double) -> [(a, a, Int, Double, Double)]
prepCollocates = fmap flatten
               . L.sortBy (comparing (^. _2 . _3))
               . M.toList

freqShowReport :: (Traversable t, Show a) => Int -> t a -> IO ()
freqShowReport n = freqReport n . frequencies . fmap show
