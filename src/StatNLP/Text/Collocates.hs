{-# LANGUAGE TupleSections #-}


module StatNLP.Text.Collocates
    ( collocates
    ) where


import           Data.Foldable
import           Data.Sequence    (ViewL (..), ViewR (..), (<|), (><), (|>))
import qualified Data.Sequence    as S
import           Data.Traversable
import qualified Data.Vector      as V

import           StatNLP.Types    hiding (left)


type CollState a = (S.Seq a, Maybe a, S.Seq a)


-- TODO: Try using StatNLP.Types.Context instead of CollState.
-- TODO: Need to load criterion and optimize this.
collocates :: (Foldable t, Traversable t)
           => Int -> Int -> t a -> [(a, a)]
collocates before after = toList
                        . uncurry (finis before)
                        . fmap fold
                        . mapAccumL (step before after) initState

initState :: CollState a
initState = (S.empty, Nothing, S.empty)

left :: S.Seq a -> S.Seq a -> S.Seq a -> CollState a
left empty nonEmpty seq =
    case S.viewl seq of
        EmptyL  -> (empty, Nothing, S.empty)
        x :< xs -> (nonEmpty, Just x, xs)

step :: Int -> Int -> CollState a -> a -> (CollState a, S.Seq (a, a))
step before after s a = let next = shift before after s a
                        in  (next, pairs next)

shift :: Int -> Int -> CollState a -> a -> CollState a
shift before after (as, current, zs) a
    | S.length zs < after = (as, current, zs |> a)
    | otherwise           = left as (trim before (maybe as (as |>) current)) (zs |> a)

shift' :: Int -> CollState a -> CollState a
shift' _      (as, Nothing, zs) = left as as zs
shift' before (as, Just c,  zs) = left as' as' zs
    where as' = trim before (as |> c)

trim :: Int -> S.Seq a -> S.Seq a
trim n ss | S.length ss > n = case S.viewl ss of
                                  EmptyL     -> S.empty
                                  (_ :< ss') -> ss'
          | otherwise       = ss

pairs :: CollState a -> S.Seq (a, a)
pairs (as, Just c, zs) = fmap (c,) $ as >< zs
pairs (_, Nothing, _)  = S.empty

finis :: Int -> CollState a -> S.Seq (a, a) -> S.Seq (a, a)
finis before s xs = xs >< finis' before (shift' before s)

finis' :: Int -> CollState a -> S.Seq (a, a)
finis' before s@(_, Nothing, zs) | S.null zs = S.empty
                                 | otherwise = finis' before (shift' before s)
finis' before s = pairs s >< finis' before (shift' before s)
