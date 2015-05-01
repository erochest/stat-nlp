{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.Text.CollocatesSpec where


import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Text.Collocates


spec :: Spec
spec = do
    describe "collocates" $ do
        let input = [0..9] :: [Int]
        {- -- It should, but I don't know what it would be.
         - it "should have a relationship between its input and output lengths." $
         -     property $ \before after (xs :: [Int]) ->
         -         let before'  = max (length xs) before
         -             after'   = max (length xs) after
         -             expected = length xs * (before' + after')
         -                      - sum ([0..before'] :: [Int])
         -                      - sum ([0..after']  :: [Int])
         -         in  length (collocates before' after' xs) == max 0 expected
         -}
        it "should work properly on short inputs." $
            collocates 1 1 ([1 , 2] :: [Int]) `shouldBe` [(1, 2), (2, 1)]
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
