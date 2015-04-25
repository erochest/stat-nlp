{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.UtilsSpec where


import           Conduit
import           Control.Monad.Identity
import qualified Data.Conduit.List      as CL
import           Data.Hashable
import qualified Data.HashMap.Strict    as M

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Utils


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

    describe "collocates" $ do
        let input = [0..9] :: [Int]
        it "should output items before its center." $
            collocates 1 0 input `shouldBe` [ (1, 0), (2, 1), (3, 2), (4, 3), (5, 4)
                                            , (6, 5), (7, 6), (8, 7), (9, 8)
                                            ]
        it "should output items after its center." $
            collocates 0 1 input `shouldBe` [ (0, 1), (1, 2), (2, 3), (3, 4), (4, 5)
                                            , (5, 6), (6, 7), (7, 8), (8, 9)
                                            ]
        it "should output items before and after its center." $
            collocates 1 1 input `shouldBe` [ (0, 1)
                                            , (1, 0), (1, 2)
                                            , (2, 1), (2, 3)
                                            , (3, 2), (3, 4)
                                            , (4, 3), (4, 5)
                                            , (5, 4), (5, 6)
                                            , (6, 5), (6, 7)
                                            , (7, 6), (7, 8)
                                            , (8, 7), (8, 9)
                                            , (9, 8)
                                            ]
        it "should output items some distance from its center." $
            collocates 3 3 input `shouldBe` [ (0, 1), (0, 2), (0, 3)
                                            , (1, 0), (1, 2), (1, 3), (1, 4)
                                            , (2, 0), (2, 1), (2, 3), (2, 4), (2, 5)
                                            , (3, 0), (3, 1), (3, 2), (3, 4), (3, 5), (3, 6)
                                            , (4, 1), (4, 2), (4, 3), (4, 5), (4, 6), (4, 7)
                                            , (5, 2), (5, 3), (5, 4), (5, 6), (5, 7), (5, 8)
                                            , (6, 3), (6, 4), (6, 5), (6, 7), (6, 8), (6, 9)
                                            , (7, 4), (7, 5), (7, 6), (7, 8), (7, 9)
                                            , (8, 5), (8, 6), (8, 7), (8, 9)
                                            , (9, 6), (9, 7), (9, 8)
                                            ]
        it "should allow you to output different before and after spans." $
            collocates 2 3 input `shouldBe` [ (0, 1), (0, 2), (0, 3)
                                            , (1, 0), (1, 2), (1, 3), (1, 4)
                                            , (2, 0), (2, 1), (2, 3), (2, 4), (2, 5)
                                            , (3, 1), (3, 2), (3, 4), (3, 5), (3, 6)
                                            , (4, 2), (4, 3), (4, 5), (4, 6), (4, 7)
                                            , (5, 3), (5, 4), (5, 6), (5, 7), (5, 8)
                                            , (6, 4), (6, 5), (6, 7), (6, 8), (6, 9)
                                            , (7, 5), (7, 6), (7, 8), (7, 9)
                                            , (8, 6), (8, 7), (8, 9)
                                            , (9, 7), (9, 8)
                                            ]

    describe "collocatesC" $ do
        let input = [0..9] :: [Int]
            collocates' b a i =  runIdentity
                              .  runConduit
                              $  CL.sourceList i
                              $= collocatesC b a
                              $$ sinkList
        it "should output items before its center." $
            collocates' 1 0 input `shouldBe` [ (1, 0), (2, 1), (3, 2), (4, 3), (5, 4)
                                             , (6, 5), (7, 6), (8, 7), (9, 8)
                                             ]
        it "should output items after its center." $
            collocates' 0 1 input `shouldBe` [ (0, 1), (1, 2), (2, 3), (3, 4), (4, 5)
                                             , (5, 6), (6, 7), (7, 8), (8, 9)
                                             ]
        it "should output items before and after its center." $
            collocates' 1 1 input `shouldBe` [ (0, 1)
                                             , (1, 0), (1, 2)
                                             , (2, 1), (2, 3)
                                             , (3, 2), (3, 4)
                                             , (4, 3), (4, 5)
                                             , (5, 4), (5, 6)
                                             , (6, 5), (6, 7)
                                             , (7, 6), (7, 8)
                                             , (8, 7), (8, 9)
                                             , (9, 8)
                                             ]
        it "should output items some distance from its center." $
            collocates' 3 3 input `shouldBe` [ (0, 1), (0, 2), (0, 3)
                                             , (1, 0), (1, 2), (1, 3), (1, 4)
                                             , (2, 0), (2, 1), (2, 3), (2, 4), (2, 5)
                                             , (3, 0), (3, 1), (3, 2), (3, 4), (3, 5), (3, 6)
                                             , (4, 1), (4, 2), (4, 3), (4, 5), (4, 6), (4, 7)
                                             , (5, 2), (5, 3), (5, 4), (5, 6), (5, 7), (5, 8)
                                             , (6, 3), (6, 4), (6, 5), (6, 7), (6, 8), (6, 9)
                                             , (7, 4), (7, 5), (7, 6), (7, 8), (7, 9)
                                             , (8, 5), (8, 6), (8, 7), (8, 9)
                                             , (9, 6), (9, 7), (9, 8)
                                             ]
        it "should allow you to output different before and after spans." $
            collocates' 2 3 input `shouldBe` ([ (0, 1), (0, 2), (0, 3)
                                              , (1, 0), (1, 2), (1, 3), (1, 4)
                                              , (2, 0), (2, 1), (2, 3), (2, 4), (2, 5)
                                              , (3, 1), (3, 2), (3, 4), (3, 5), (3, 6)
                                              , (4, 2), (4, 3), (4, 5), (4, 6), (4, 7)
                                              , (5, 3), (5, 4), (5, 6), (5, 7), (5, 8)
                                              , (6, 4), (6, 5), (6, 7), (6, 8), (6, 9)
                                              , (7, 5), (7, 6), (7, 8), (7, 9)
                                              , (8, 6), (8, 7), (8, 9)
                                              , (9, 7), (9, 8)
                                              ] :: [(Int, Int)])
