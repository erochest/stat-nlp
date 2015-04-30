{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.Text.UtilsSpec where


import           Control.Monad.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict    as M
import qualified Data.List              as L

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Text.Utils


instance (Eq k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (M.HashMap k v) where
    arbitrary = M.fromList <$> arbitrary
    shrink = map M.fromList . shrink . M.toList


spec :: Spec
spec = do
    describe "count" $ do
        it "should expand when a new item is added to it." $
            M.size (count M.empty (42 :: Int)) `shouldBe` 1
        it "should maintain its size when an existing item is re-added to it." $
            M.size (count [(42, 1)] (42 :: Int)) `shouldBe` 1

    describe "frequencies" $
        it "should have the sum of its values equal to the length of its center." $
            property $ \(xs :: [Int]) -> sum (M.elems (frequencies xs)) == length xs

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
