{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module StatNLP.Statistics.Bayes where


import           Control.Arrow
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict            as M
import qualified Data.HashSet                   as S
import qualified Data.List                      as L
import           Data.Maybe
import           Data.Monoid
import           Data.Ord

import           StatNLP.Distributions.Lidstone
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

type FeatureValues f v = M.HashMap f (S.HashSet v)

data BayesTrain l f v
    = BayesTrain
    { trainLabel   :: FreqMap l
    -- ^ label_freqdist
    , trainValue   :: FeatureValues f (Maybe v)
    -- ^ feature_values
    -- ^ fnames = S.HashSet . M.keys $ trainValue bayes
    , trainFeature :: ConditionalFreq (l, f) (Maybe v)
    -- ^ feature_freqdist
    }

data BayesDist l f v
    = BayesDist
      { bayesLabelDist   :: Lidstone l
      , bayesFeatureDist :: M.HashMap (l, f) (Lidstone v)
      }

instance ( Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v
         ) => Monoid (BayesTrain l f v) where

    mempty = BayesTrain mempty mempty mempty
    (BayesTrain al av af) `mappend` (BayesTrain bl bv bf) =
        BayesTrain (al `mappend` bl) (av `mappend` bv) (af `mappend` bf)

trainOne :: (Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
            => BayesTrain l f v        -- ^ The Bayesian trainer
                -> l                   -- ^ The label
                -> [(f, v)]            -- ^ Features and values
                -> BayesTrain l f v
trainOne BayesTrain{..} l fs =
    BayesTrain { trainLabel = count trainLabel l
               , trainValue = foldl' step trainValue $ fmap (fmap Just) fs
               , trainFeature =
                   countAll trainFeature [((l, f), Just v) | (f, v) <- fs]
               }
    where
      step :: (Hashable f, Eq f, Hashable v, Eq v)
              => FeatureValues f v -> (f, v)
                                   -> FeatureValues f v
      step m (f, v) = M.insert f (S.insert v $ M.lookupDefault S.empty f m) m

train :: (Traversable t, Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
      => BayesTrain l f v -> t (l, [(f, v)]) -> BayesTrain l f v
train = foldl' step
    where
      step m (l, fs) = trainOne m l fs

finishTraining :: (Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
                  => BayesTrain l f v
                      -> Either DistributionException (BayesDist l f (Maybe v))
finishTraining BayesTrain{..} =
    BayesDist <$> lidstoneProbDist trainLabel 0.5 Nothing <*> featureProbDist
    where
      labels   = M.keys $ unHash trainLabel
      features = M.keys trainValue

      counts = do
        l <- labels
        let numSamples = frequency trainLabel l
        f <- features
        let pair = (l, f)
            c = total . M.lookupDefault mempty pair $ unHash trainFeature
            diff = numSamples - c
        guard (diff > 0)
        return (pair, diff)

      (tValue, tFeature) = foldl' step (trainValue, trainFeature) counts

      step :: (Eq m, Hashable m, Eq g, Hashable g, Eq w, Hashable w)
              => (FeatureValues g (Maybe w), ConditionalFreq (m, g) (Maybe w))
                  -> ((m, g), Int)
                  -> ( FeatureValues g (Maybe w)
                     , ConditionalFreq (m, g) (Maybe w))
      step (vs, fs) (pair@(_, f), c) = ( insertFeatureValue vs f Nothing
                                       , incConditionalFreq fs pair Nothing c
                                       )

      featureProbDist = fmap M.fromList
                        . mapM (uncurry makeProbDist)
                        . M.toList
                        $ unHash trainFeature

      makeProbDist p@(l, f) fm =
          sequenceA . (p,)
                        . lidstoneProbDist fm 0.5
                        . Just
                        . S.size
                        $ M.lookupDefault mempty f trainValue

insertFeatureValue :: (Eq f, Hashable f, Eq v, Hashable v)
                      => FeatureValues f v -> f -> v -> FeatureValues f v
insertFeatureValue fvs f v =
    M.insert f (S.insert v $ M.lookupDefault mempty f fvs) fvs

incConditionalFreq :: (Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
                      => ConditionalFreq (l, f) v -> (l, f) -> v -> Int
                                                  -> ConditionalFreq (l, f) v
incConditionalFreq (MHash cf) k v c =
    MHash . flip (M.insert k) cf
              . MHash
              . M.insertWith (+) v (Sum c)
              . unHash
              $ M.lookupDefault mempty k cf

categorizeP :: (Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
            => BayesDist l f v -> M.HashMap f v -> ProbMap l
categorizeP BayesDist{..} fs =
    undefined
    -- flip (foldl' step) fs . fmap log2 $ M.lookupDefault mempty w bayesPrior
    where
      labels = M.keys . unHash . lidstoneFD $ bayesLabelDist
      features = M.keys fs
      missing = do
        f <- features
        let labelHits = [l | l <- labels, (l, f) `M.member` bayesFeatureDist]
        guard (L.null labelHits)
        return f
      fs' = foldl' (flip M.delete) fs missing
      logProgs = M.fromList
                 $ mapMaybe ( sequenceA
                            . (id &&& logProbability bayesLabelDist)
                            )
                 labels
      logProgs' = foldl' logStep logProgs . catMaybes . fmap join $ do
                    l <- labels
                    (f, v) <- M.toList fs'
                    let key = (l, f)
                    return $ sequenceA . (l,) . (`logProbability` v)
                               <$> M.lookup (l, f) bayesFeatureDist
      logStep m (l, v) = M.insertWith (+) l v m

categorize :: (Eq l, Hashable l, Eq f, Hashable f, Eq v, Hashable v)
           => BayesDist l f v -> M.HashMap f v -> Maybe l
categorize l fs = listToMaybe
                  . fmap fst
                  . L.sortBy (comparing (Down . snd))
                  . M.toList
                  $ categorizeP l fs
