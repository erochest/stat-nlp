{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Arrow
-- import           Control.Monad
import           Data.Foldable
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import qualified Data.List                as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                as T
-- import qualified Data.Text.Format         as F
import qualified Data.Text.IO             as TIO
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as MV
import           System.FilePath
import           System.Random.MWC

import           StatNLP.Corpus
import           StatNLP.Document
import           StatNLP.Statistics.Bayes
import           StatNLP.Text.Freqs
-- import qualified StatNLP.Text.Freqs       as Freqs
import           StatNLP.Text.Tokens
import           StatNLP.Types

import           Opts


-- For Naive Bayesian spam filter:
--
-- x. Walk over the input directory;
-- x. Tokenize, normalize, and stoplist the text;
-- x. Tag from the filename;
-- x. Randomize the documents;
-- x. Divide them into equal sections for cross validation
-- (https://en.wikipedia.org/wiki/Cross-validation_%28statistics%29, maybe
-- do this as a library as well);
-- 6. For each pass, train on 9 sections, test on 1, and compute the
-- error;
--    * [MSE](https://en.wikipedia.org/wiki/Mean_squared_error);
--    * https://en.wikipedia.org/wiki/Accuracy_and_precision;
--    * https://en.wikipedia.org/wiki/Perplexity; and
--    * https://en.wikipedia.org/wiki/F1_score;
-- 7. Average the errors.

main :: IO ()
main = do
    Args{..} <- parseArgs
    stopwords <- maybe (return S.empty)
                       ( fmap (S.fromList . fmap _tokenNorm . tokenize)
                       . TIO.readFile)
                       stopList
    parts <-  fmap (split 10)
          .   withSystemRandom
          .   asGenIO
          .   flip shuffle
          .   fmap (documentToTraining . fmap (fmap _tokenNorm))
          .   V.fromList
          .   M.elems
          =<< readCorpusVectors Nothing
          =<< loadCorpusDirectory (tokenizer' stopwords)
                                  (TIO.readFile . _documentId)
                                  (return . tagDocument)
                                  inputDir

    mapM_ print
        . fmap (uncurry testBayes . fmap V.concat)
        $ walkPartitions parts

type Training = (T.Text, [T.Text], [T.Text])

walkPartitions :: [x] -> [(x, [x])]
walkPartitions xs = walk' xs []
    where
        walk' []     _    = []
        walk' (y:ys) prev = (y, prev ++ ys) : walk' ys (y:prev)

-- TODO: Need to actually test for the PPV by looking for positive and
-- negatives (can either define one tag as positive or change tags to +/-.)
--
-- TODO: Can actually cache the training of these by training each
-- partition once and combining the training sets at this point.
--
-- TODO: Move a lot of this into the library.
testBayes :: V.Vector Training -> V.Vector Training -> (Int, Int)
testBayes testSet trainingSet =
    -- uncurry (/)
    (getSum *** getSum)
        . foldMap (   ( Sum . testMetric)
                  &&& ( Sum
                      . fromIntegral
                      . getSum
                      . fold
                      . (`M.lookup` cfreqs)
                      . fst))
        . V.toList
        $ fmap (testTraining . finishTraining $ train mempty trainingSet) testSet
    where
        testTraining :: BayesDist T.Text T.Text T.Text -> Training
                     -> (T.Text, T.Text)
        testTraining bayes (w, [t], fs) = (t,) . fold $ categorize bayes w fs
        testTraining _     _            = mempty

        MHash cfreqs = frequencies $ concatMap (\(_, ts, _) -> ts) testSet

        testMetric (a, b) = if a == b then 1 else 0

tagDocument :: Document b ts -> Document b ts
tagDocument d@Document{..} =
    d { _documentTags = S.singleton . tag $ takeFileName _documentId }
    where
        tag ('s':'p':'m':_) = "SPAM"
        tag _               = "HAM"

documentToTraining :: Document b (V.Vector PlainToken)
                   -> (T.Text, [T.Text], [T.Text])
documentToTraining d@Document{..} =
    ( documentKey d
    , S.toList _documentTags
    , S.toList . S.fromList $ V.toList _documentTokens
    )

-- | Fisher-Yates shuffle, modern algorithm
--
-- If the random number generator can return a random integer p ≤ j < q for
-- specified parameters p, q then following version could be used:
--
-- To shuffle an array a of n elements (indices 0..n-1):
--   for i from 0 to n − 2 do
--        j ← random integer such that i ≤ j < n
--        exchange a[j] and a[i]
shuffle :: GenIO -> V.Vector x -> IO (V.Vector x)
shuffle g v' = do
    v <- V.thaw v'
    let n = MV.length v

    forM_ [0..(n-2)] $ \i -> do
        j <- uniformR (i+1, n-1) g
        MV.swap v i j

    V.freeze v

split :: Int -> V.Vector x -> [V.Vector x]
split chunks vs =
    snd .  mapAccumL step vs
        $  L.replicate overflow (size + 1)
        ++ L.replicate (chunks - overflow) size
    where
        (size, overflow) = V.length vs `divMod` chunks
        step input n = swap $ V.splitAt n input

tokenizer' :: StopWords -> Tokenizer (Token LinePos PlainToken)
tokenizer' = tokenizerStop

sortOn :: (a, b, c, d) -> (a, Down c, b)
sortOn (a, b, c, _) = (a, Down c, b)

flatten :: (a, (b, c)) -> (a, b, c)
flatten (a, (b, c)) = (a, b, c)
