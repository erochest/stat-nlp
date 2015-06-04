{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module StatNLP.Text.CollocatesSpec where


import           Conduit
import qualified Data.List               as L
import           Data.Ord
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Text.Collocates
import           StatNLP.Types


-- Since everything's a list of ascending integers, this works.
c :: (Int, Int) -> Collocate Int
c (x, y) = Collocate x y (y - x)

totuple :: Collocate a -> (a, a, Int)
totuple (Collocate a b c) = (a, b, c)

cs :: [(Int, Int)] -> [Collocate Int]
cs = L.sortBy (comparing totuple) . fmap c

collocates' :: Int -> Int -> [Int] -> [Collocate Int]
collocates' i j xs = L.sortBy (comparing totuple) $ collocates i j xs

collocatesC' :: Int -> Int -> [Int] -> [Collocate Int]
collocatesC' i j xs =  fmap (\(Collocate a b d) -> Collocate (tread a) (tread b) d)
                    .  runIdentity
                    $  yieldMany xs'
                    =$ getCollocatesC i j
                    $$ sinkList
    where
        doc = Document "<collocatesC'>" mempty Nothing
            . V.fromList
            . zipWith token [0..]
            $ fmap show xs
        xs' :: [(Int, VectorDoc b)]
        xs' = fmap (, doc) [0 .. (length xs) - 1]
        token n t = Token (T.pack t) Nothing n
        tread = read . T.unpack

around' :: Int -> Int -> Int -> V.Vector Int -> [Collocate Int]
around' i j n xs = L.sortBy (comparing totuple) $ collocatesAround i j n xs

spec :: Spec
spec = do
    let input  = [0..9] :: [Int]
        vinput = V.fromList input

    describe "collocates" $ do
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
            collocates' 1 1 ([1 , 2] :: [Int]) `shouldBe` cs [(1, 2), (2, 1)]
        it "should output items before its center." $
            collocates' 1 0 input `shouldBe` cs [ (1, 0), (2, 1), (3, 2), (4, 3), (5, 4)
                                                , (6, 5), (7, 6), (8, 7), (9, 8)
                                                ]
        it "should output items after its center." $
            collocates' 0 1 input `shouldBe` cs [ (0, 1), (1, 2), (2, 3), (3, 4), (4, 5)
                                                , (5, 6), (6, 7), (7, 8), (8, 9)
                                                ]
        it "should output items before and after its center." $
            collocates' 1 1 input `shouldBe` cs [ (0, 1)
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
            collocates' 3 3 input `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
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
            collocates' 2 3 input `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
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
        it "should allow you to output the distance before and after." $
            collocates' 3 3 [0..3] `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
                                                 , (1, 2), (1, 3), (1, 0)
                                                 , (2, 3), (2, 0), (2, 1)
                                                 , (3, 0), (3, 1), (3, 2)
                                                 ]

    describe "collocatesAround" $
        it "should allow you to output the distance before and after." $
            around' 3 3 5 vinput `shouldBe` cs [ (5, 2), (5, 3), (5, 4)
                                               , (5, 6), (5, 7), (5, 8)
                                               ]

    describe "getCollocatesC" $ do
        it "should work properly on short inputs." $
            collocatesC' 1 1 ([1 , 2] :: [Int]) `shouldBe` cs [(1, 2), (2, 1)]
        it "should output items before its center." $
            collocatesC' 1 0 input `shouldBe` cs [ (1, 0), (2, 1), (3, 2), (4, 3), (5, 4)
                                                 , (6, 5), (7, 6), (8, 7), (9, 8)
                                                 ]
        it "should output items after its center." $
            collocatesC' 0 1 input `shouldBe` cs [ (0, 1), (1, 2), (2, 3), (3, 4), (4, 5)
                                                 , (5, 6), (6, 7), (7, 8), (8, 9)
                                                 ]
        it "should output items before and after its center." $
            collocatesC' 1 1 input `shouldBe` cs [ (0, 1)
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
            collocatesC' 3 3 input `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
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
            collocatesC' 2 3 input `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
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
        it "should allow you to output the distance before and after." $
            collocatesC' 3 3 [0..3] `shouldBe` cs [ (0, 1), (0, 2), (0, 3)
                                                  , (1, 2), (1, 3), (1, 0)
                                                  , (2, 3), (2, 0), (2, 1)
                                                  , (3, 0), (3, 1), (3, 2)
                                                  ]
