{-# LANGUAGE TupleSections #-}


module StatNLP.Utils where


import           Conduit
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.Sequence       (ViewL (..), ViewR (..), (<|), (><), (|>))
import qualified Data.Sequence       as S
import           Data.Traversable

import           StatNLP.Types


count :: (Eq a, Hashable a) => FreqMap a -> a -> FreqMap a
count m a = M.insertWith (+) a 1 m

frequencies :: (Eq a, Hashable a, Foldable f) => f a -> FreqMap a
frequencies = foldl' count M.empty

frequenciesC :: (Monad m, Eq a, Hashable a) => Consumer a m (FreqMap a)
frequenciesC = foldlC count M.empty

tokenTypeRatio :: FreqMap a -> Double
tokenTypeRatio fm = fromIntegral (sum (M.elems fm))
                  / fromIntegral (M.size fm)

type CollState a = (S.Seq a, Maybe a, S.Seq a)

collocates :: Int -> Int -> [a] -> [(a, a)]
collocates before after = uncurry finis . mapAccumL step init
    where
        init :: CollState a
        init = (S.empty, Nothing, S.empty)

        step :: CollState a -> a -> (CollState a, [(a, a)])
        step s a = let next = shift s a
                   in  (next, pairs next)

        shift :: CollState a -> a -> CollState a
        shift (as, current, zs) a
            | zlen < after = (as, current, zs |> a)
            | otherwise
                = case S.viewl (zs |> a) of
                      EmptyL          -> (as, Nothing, S.empty)
                      current' :< zs' -> ( trim before (maybe as (as |>) current)
                                         , Just current'
                                         , zs'
                                         )
            where
                alen = S.length as
                zlen = S.length zs

        shift' :: CollState a -> CollState a
        shift' (as, Nothing, zs) =
            case S.viewl zs of
                EmptyL   -> (as, Nothing, S.empty)
                c :< zs' -> (as, Just c, zs')
        shift' (as, Just c, zs) =
            case S.viewl zs of
                EmptyL    -> (cycle as c, Nothing, S.empty)
                c' :< zs' -> (cycle as c, Just c', zs')

        cycle :: S.Seq a -> a -> S.Seq a
        cycle xs x = case S.viewl (xs |> x) of
                         EmptyL   -> S.empty
                         _ :< xs' -> xs'

        trim :: Int -> S.Seq a -> S.Seq a
        trim n ss | S.length ss > n = case S.viewl ss of
                                          EmptyL     -> S.empty
                                          (_ :< ss') -> ss'
                  | otherwise       = ss

        pairs :: CollState a -> [(a, a)]
        pairs (as, Just c, zs) = toList . fmap (c,) $ as >< zs
        pairs (_, Nothing, _)  = []

        finis :: CollState a -> [[(a, a)]] -> [(a, a)]
        finis s xs = concat xs ++ finis' (shift' s)

        finis' :: CollState a -> [(a, a)]
        finis' s@(_, Nothing, zs) | S.null zs = []
                                  | otherwise = finis' (shift' s)
        finis' s = pairs s ++ finis' (shift' s)
