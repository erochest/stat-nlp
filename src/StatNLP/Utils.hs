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
collocates before after = uncurry finis . mapAccumL (step before after) initState

initState :: CollState a
initState = (S.empty, Nothing, S.empty)

left :: S.Seq a -> S.Seq a -> S.Seq a -> CollState a
left empty nonEmpty seq =
    case S.viewl seq of
        EmptyL  -> (empty, Nothing, S.empty)
        x :< xs -> (nonEmpty, Just x, xs)

step :: Int -> Int -> CollState a -> a -> (CollState a, [(a, a)])
step before after s a = let next = shift before after s a
                        in  (next, pairs next)

shift :: Int -> Int -> CollState a -> a -> CollState a
shift before after (as, current, zs) a
    | S.length zs < after = (as, current, zs |> a)
    | otherwise           = left as (trim before (maybe as (as |>) current)) (zs |> a)

shift' :: CollState a -> CollState a
shift' (as, Nothing, zs) = left as as zs
shift' (as, Just c,  zs) = left as' as' zs
    where as' = case S.viewl (as |> c) of
                    EmptyL  -> S.empty
                    _ :< xs -> xs

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

collocatesC :: Monad m => Int -> Int -> Conduit a m (a, a)
collocatesC before after = go initState
    where
        go s = do
            x <- await
            case x of
                Just x' -> do
                    let (s', xs) = step before after s x'
                    yieldMany xs
                    go s'
                Nothing -> yieldMany . finis' $ shift' s
