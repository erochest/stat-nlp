{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.Statistics.Bayes where


import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Traversable

import           StatNLP.Text.Freqs
import           StatNLP.Types


{- Pseudo code:
-
- comment: training
- for all senses Sk of W do
-   for all words Vj in the vocabulary do
-      P(Vj | Sk) = C(Vj, Sk) / C(Vj)
-   end
- end
- for all sense Sk of w do
-   P(Sk) = C(Sk) / C(W)
- end
-
- comment: disabiguation
- for all sense Sk of W do
-   score (Sk) = log P(Sk)
-   for all word Vj in the context window C do
-     score(Sk) += log P(Vj | Sk)
-   end
- end
- choose s' = arg max{Sk} score(Sk)
-
- * C(Vj, Sk) = frequency of Vj in the context of sense Sk
- * C(Sk)     = frequency of Sk in the training corpus
- * C(W)      = frequency of ambiguous word W (all senses)
-
-}

data BayesTrain w s f
        = BayesTrain
        { trainWord         :: FreqMap w
        , trainCat          :: FreqMap s
        , trainFeature      :: FreqMap f
        , trainFeatureSense :: ConditionalFreq s f
        }

data BayesDist w s f
        = BayesDist
        { bayesPrior :: ConditionalProbMap w s
        , bayesPost  :: ConditionalProbMap s f
        }

instance (Eq w, Hashable w, Eq s, Hashable s, Eq f, Hashable f) => Monoid (BayesTrain w s f) where
    mempty = BayesTrain mempty mempty mempty mempty
    (BayesTrain aw as af asf) `mappend` (BayesTrain bw bs bf bsf) =
        BayesTrain (aw `mappend` bw) (as `mappend` bs) (af `mappend` bf) (asf `mappend` bsf)

trainOne :: (Eq w, Hashable w, Eq s, Hashable s, Eq f, Hashable f)
         => BayesTrain w s f -> w -> [s] -> [f] -> BayesTrain w s f
trainOne BayesTrain{..} w ss fs =
    BayesTrain { trainWord         = count    trainWord         w
               , trainCat          = countAll trainCat          ss
               , trainFeature      = countAll trainFeature      fs
               , trainFeatureSense = countAll trainFeatureSense [(s, f) | s <- ss, f <- fs]
               }

train :: (Traversable t, Eq w, Hashable w, Eq s, Hashable s, Eq f, Hashable f)
      => BayesTrain w s f -> t (w, [s], [f]) -> BayesTrain w s f
train bt = foldl' (uncurry3 . trainOne) bt
    where
        uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
        uncurry3 f (a, b, c) = f a b c

finishTraining :: BayesTrain w s f -> BayesDist w s f
finishTraining BayesTrain{..} = BayesDist prior post
    where
        sensePriors sfreq wordCount = fmap ((/wordCount) . fromIntegral . getSum) $ unHash sfreq
        prior = fmap (sensePriors trainCat . fromIntegral . getSum) $ unHash trainWord
        post = fmap probabilities $ unHash trainFeatureSense

categorizeP :: (Eq w, Hashable w, Eq s, Hashable s, Eq f, Hashable f)
            => BayesDist w s f -> w -> [f] -> ProbMap s
categorizeP BayesDist{..} w fs =
    flip (foldl' step) fs . fmap log2 $ M.lookupDefault mempty w bayesPrior
    where
        log2 x = logBase x 2
        step p f = M.mapWithKey (update f) p
        update f s p = p + (log2 . fromMaybe 0.0) (M.lookup f =<< M.lookup s bayesPost)

categorize :: (Eq w, Hashable w, Eq s, Hashable s, Eq f, Hashable f)
           => BayesDist w s f -> w -> [f] -> Maybe s
categorize b w fs = listToMaybe
                  . fmap fst
                  . L.sortBy (comparing (Down . snd))
                  . M.toList
                  $ categorizeP b w fs
