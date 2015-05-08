{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}


module StatNLP.Context
    ( pushL
    , pushR
    , shiftL
    , shiftR
    , trimL
    , trimR

    , pushContext
    , appendContext
    , getContext
    ) where


import qualified Data.FingerTree  as FT
import           Data.Foldable
import           Data.Monoid
import qualified Data.Sequence    as Seq
import           Data.Traversable

import           StatNLP.Types


pushL :: Context a -> a -> Context a
pushL (Context b a ls c rs) x =
    case Seq.viewl rs of
        Seq.EmptyL   -> Context b a ls' x rs
        r Seq.:< rs' -> Context b a ls' r $ rs' Seq.|> x
    where
        ls' = (ls Seq.|> c) `trimL` b

pushR :: a -> Context a -> Context a
pushR x (Context b a ls c rs) =
    case Seq.viewr ls of
        Seq.EmptyR   -> Context b a ls x rs'
        ls' Seq.:> l -> Context b a (x Seq.<| ls') l rs'
    where
        rs' = (c Seq.<| rs) `trimR` a

trimL :: Seq.Seq a -> Int -> Seq.Seq a
trimL ss n | Seq.length ss > n = case Seq.viewl ss of
                                     Seq.EmptyL   -> ss
                                     _ Seq.:< ss' -> trimL ss' n
           | otherwise         = ss

trimR :: Seq.Seq a -> Int -> Seq.Seq a
trimR ss n | Seq.length ss > n = case Seq.viewr ss of
                                     Seq.EmptyR   -> ss
                                     ss' Seq.:> _ -> trimR ss' n
           | otherwise         = ss

shiftL, shiftR :: Context a -> Maybe (Context a)
shiftL (Context a b ls c rs) =
    case Seq.viewr rs of
        Seq.EmptyR   -> Nothing
        rs' Seq.:> r -> Just $ Context a b (ls Seq.|> c) r rs'

shiftR (Context a b ls c rs) =
    case Seq.viewl ls of
        Seq.EmptyL   -> Nothing
        l Seq.:< ls' -> Just $ Context a b ls' l (c Seq.<| rs)

pushContext :: FT.Measured (Sum Int) a => MeasuredContext a -> a -> MeasuredContext a
pushContext (MContext size ftree) a = MContext size
                                    . uncurry shiftLeft
                                    . FT.split (>size)
                                    $ CItem a FT.<| ftree

getContext :: FT.Measured (Sum Int) a => MeasuredContext a -> [a]
getContext = fmap getContextItem . toList . FT.reverse . mContextSeq

appendContext :: (FT.Measured (Sum Int) a, Foldable t, Traversable t)
              => MeasuredContext a -> t a -> MeasuredContext a
appendContext (MContext size ftree) = MContext size
                                    . uncurry shiftLeft
                                    . FT.split (>size)
                                    . (<> ftree)
                                    . FT.fromList
                                    . reverse
                                    . toList
                                    . fmap CItem

shiftLeft :: FT.Measured v a => FT.FingerTree v a -> FT.FingerTree v a -> FT.FingerTree v a
shiftLeft ft1 (FT.viewl -> x FT.:< _) = ft1 FT.|> x
shiftLeft ft  _                       = ft
