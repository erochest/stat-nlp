{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.Text.UtilsSpec where


import           Control.Monad.Identity
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L
import           Data.Monoid
import           Data.Ord

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Text.Freqs
import           StatNLP.Text.Utils
import           StatNLP.Types


instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (M.HashMap k v) where
    arbitrary = M.fromList <$> arbitrary
    shrink = map M.fromList . shrink . M.toList


spec :: Spec
spec = do
    describe "count" $ do
        it "should expand when a new item is added to it." $
            (M.size . unHash $ count mempty (42 :: Int)) `shouldBe` 1
        it "should maintain its size when an existing item is re-added to it." $
            (M.size . unHash . count (MHash [(42 :: Int, 1)]) $ 42 :: Int) `shouldBe` 1

    describe "frequencies" $
        it "should have the sum of its values equal to the length of its input." $
            property
                $ \(xs :: [Int]) -> ( getSum
                                    . sum
                                    . M.elems
                                    . unHash
                                    $ frequencies xs
                                    ) == length xs

    describe "frequencyBy" $ do
        it "should have the sum of its values equal to the length of its input." $
            property $ \(xs :: [(Char, Int)]) ->
                ( getSum
                . foldMap (fold . M.elems . unHash)
                . M.elems
                . unHash
                $ frequencyBy fst snd xs) == length xs
        it "should have the sum of its group frequencies equal to its group totals." $
            property $ \(xs :: [(Char, Int)]) ->
                let charf  = unHash . frequencies $ map fst xs
                    groupf = unHash $ frequencyBy fst snd xs
                    groupSum (k, ex) = ex == ( fold
                                             . M.elems
                                             . unHash
                                             . fold
                                             $ M.lookup k groupf
                                             )
                in  M.size charf == M.size groupf && all groupSum (M.toList charf)
        it "should properly count the grouped items." $
            let input =    [ ('a', 1), ('b', 2), ('c', 3)
                           , ('a', 1), ('b', 2), ('c', 3)
                           , ('a', 1), ('b', 2), ('c', 3)
                           , ('a', 1), ('b', 2), ('c', 3)
                           , ('a', 2), ('b', 3), ('c', 4)
                           ] :: [(Char, Int)]
                expected = [ ('a', [(1, Sum 4), (2, Sum 1)])
                           , ('b', [(2, Sum 4), (3, Sum 1)])
                           , ('c', [(3, Sum 4), (4, Sum 1)])
                           ] :: [(Char, [(Int, Sum Int)])]
            in expected == ( (fmap . fmap) (L.sort . M.toList . unHash)
                           . L.sortBy (comparing fst)
                           . M.toList
                           . unHash
                           $ frequencyBy fst snd input
                           )

    describe "ngrams" $ do
        it "should return sublists of equal length." $
            property $ \n (xs :: [Int]) ->
                case map length (ngrams n xs) of
                    []   -> True
                    [_]  -> True
                    y:ys -> all (== y) ys
        it "should return all the sublists of a sequence." $
            ngrams 3 ([0..9] :: [Int]) `shouldBe` [ [0, 1, 2], [1, 2, 3]
                                                  , [2, 3, 4], [3, 4, 5]
                                                  , [4, 5, 6], [5, 6, 7]
                                                  , [6, 7, 8], [7, 8, 9]
                                                  ]
