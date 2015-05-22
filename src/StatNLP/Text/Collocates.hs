{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}


module StatNLP.Text.Collocates
    ( collocates
    , collocatePairs
    , collocatesAround
    , collocatePairsAround
    , collocateStats
    ) where


import           Control.Arrow       ((&&&))
import           Control.Monad       (ap)
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Sequence       (ViewL (..), ViewR (..), (<|), (><), (|>))
import qualified Data.Sequence       as S
import           Data.Traversable
import           Data.Tuple          (swap)
import           Data.Vector         ((!?))
import qualified Data.Vector         as V
import           Statistics.Sample

import           StatNLP.Types       hiding (left)


data CollState a
        = CS
        { csBefore :: !(S.Seq (a, Int))
        , csCenter :: !(Maybe (a, Int))
        , csAfter  :: !(S.Seq (a, Int))
        , csI      :: !Int
        }

instance Enum (CollState a) where
    succ cs@CS{csI} = cs { csI = succ csI }
    pred cs@CS{csI} = cs { csI = pred csI }
    toEnum   = CS S.empty Nothing S.empty
    fromEnum = csI
    enumFrom x           = map toEnum [csI x ..]
    enumFromThen x y     = map toEnum [csI x, csI y .. ]
    enumFromTo x y       = map toEnum [csI x .. csI y]
    enumFromThenTo x y z = map toEnum [csI x, csI y .. csI z]


-- | The values in the output @M.HashMap@ are the (N, mean, variance).
collocateStats :: (Eq a, Hashable a, Foldable f)
               => f (Collocate a) -> M.HashMap (a, a) (Int, Double, Double)
collocateStats = fmap (stats . fmap fromIntegral . V.fromList . toList)
               . unHash
               . foldMap singleton
    where
        singleton (Collocate x y d) = MHash . M.singleton (x, y) $ S.singleton d
        stats vs = (V.length vs, mean vs, variance vs)

collocates :: (Foldable t, Traversable t)
           => Int -> Int -> t a -> [Collocate a]
collocates before after = toList
                        . uncurry (finis before)
                        . fmap fold
                        . mapAccumL (step before after) initState

collocatePairs :: (Foldable t, Traversable t)
               => Int -> Int -> t a -> [(a, a)]
collocatePairs before after = fmap toPair . collocates before after

collocatesAround :: Int -> Int -> Int -> V.Vector a -> [Collocate a]
collocatesAround before after center v =
    mapMaybe ( ap (fmap (uncurry . Collocate) mcenter)
             . fmap swap
             . sequenceA
             . fmap (v !?)
             . (id &&& (+center))
             )
        $ filter (/=0) [(-before) .. after]
    where
        mcenter = v !? center

collocatePairsAround :: Int -> Int -> Int -> V.Vector a -> [(a, a)]
collocatePairsAround before after center =
    fmap toPair . collocatesAround before after center

toPair :: Collocate a -> (a, a)
toPair (Collocate a b _) = (a, b)

initState :: CollState a
initState = CS S.empty Nothing S.empty 0

left :: S.Seq (a, Int) -> S.Seq (a, Int) -> S.Seq (a, Int) -> Int -> CollState a
left empty nonEmpty seq n =
    case S.viewl seq of
        EmptyL  -> CS empty    Nothing  S.empty n
        x :< xs -> CS nonEmpty (Just x) xs      n

step :: Int -> Int -> CollState a -> a -> (CollState a, S.Seq (Collocate a))
step before after s a = let next = shift before after s (a, csI s)
                        in  (succ next, pairs next)

shift :: Int -> Int -> CollState a -> (a, Int) -> CollState a
shift before after cs@(CS as current zs n) a
    | S.length zs < after = cs { csAfter = zs |> a }
    | otherwise           = left as (trim before (maybe as (as |>) current)) (zs |> a) n

shift' :: Int -> CollState a -> CollState a
shift' _      (CS as Nothing  zs n) = left as  as  zs n
shift' before (CS as (Just c) zs n) = left as' as' zs n
    where as' = trim before (as |> c)

trim :: Int -> S.Seq a -> S.Seq a
trim n ss | S.length ss > n = case S.viewl ss of
                                  EmptyL     -> S.empty
                                  (_ :< ss') -> ss'
          | otherwise       = ss

pairs :: CollState a -> S.Seq (Collocate a)
pairs (CS _ Nothing _ _)    = S.empty
pairs (CS as (Just c) zs _) = fmap (coll c) $ as >< zs
    where
        coll (c, i) (d, j) = Collocate c d (j - i)

finis :: Int -> CollState a -> S.Seq (Collocate a) -> S.Seq (Collocate a)
finis before s xs = xs >< finis' before (shift' before s)

finis' :: Int -> CollState a -> S.Seq (Collocate a)
finis' before s@(CS _ Nothing zs _) | S.null zs = S.empty
                                    | otherwise = finis' before (shift' before s)
finis' before s = pairs s >< finis' before (shift' before s)
