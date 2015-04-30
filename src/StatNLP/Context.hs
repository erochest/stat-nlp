
module StatNLP.Context
    ( pushL
    , pushR
    , shiftL
    , shiftR
    , trimL
    , trimR
    ) where


import           Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence as Seq

import           StatNLP.Types


pushL :: Context a -> a -> Context a
pushL (Context b a ls c rs) x =
    case Seq.viewl rs of
        EmptyL   -> Context b a ls' x rs
        r :< rs' -> Context b a ls' r $ rs' |> x
    where
        ls' = (ls |> c) `trimL` b

pushR :: a -> Context a -> Context a
pushR x (Context b a ls c rs) =
    case Seq.viewr ls of
        EmptyR   -> Context b a ls x rs'
        ls' :> l -> Context b a (x <| ls') l rs'
    where
        rs' = (c <| rs) `trimR` a

trimL :: Seq a -> Int -> Seq a
trimL ss n | Seq.length ss > n = case Seq.viewl ss of
                                     EmptyL   -> ss
                                     _ :< ss' -> trimL ss' n
           | otherwise         = ss

trimR :: Seq a -> Int -> Seq a
trimR ss n | Seq.length ss > n = case Seq.viewr ss of
                                     EmptyR   -> ss
                                     ss' :> _ -> trimR ss' n
           | otherwise         = ss

shiftL, shiftR :: Context a -> Maybe (Context a)
shiftL (Context a b ls c rs) =
    case Seq.viewr rs of
        EmptyR   -> Nothing
        rs' :> r -> Just $ Context a b (ls |> c) r rs'

shiftR (Context a b ls c rs) =
    case Seq.viewl ls of
        EmptyL   -> Nothing
        l :< ls' -> Just $ Context a b ls' l (c <| rs)

