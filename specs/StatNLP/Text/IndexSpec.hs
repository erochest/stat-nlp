{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.Text.IndexSpec where


import qualified Data.List           as L
import           Data.Maybe
import           Data.Text
import qualified Data.Text           as T

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Specs.Utils
import           StatNLP.Text.Index
import qualified StatNLP.Text.Index  as I
import           StatNLP.Types


uniqueCount :: Eq a => [a] -> Int
uniqueCount = L.length . L.nub


spec :: Spec
spec = do
    describe "empty" $
        it "should return an index with no items." $ do
            size   I.empty `shouldBe` 0
            toList I.empty `shouldBe` ([] :: [Int])

    describe "singleton" $
        it "should return an index with one item." $ property $ \(t :: Text) ->
            let i = I.singleton t
            in  size i == 1 && toList i == [t]

    describe "null" $ do
        it "should return true for empty indexes." $
            I.null I.empty `shouldBe` True
        it "should return false for other inputs." $ property $ \(ts :: [Text]) ->
            I.null (fromList ts) == L.null ts

    describe "size" $ do
        it "should return 0 for empty indexes." $
            size I.empty `shouldBe` 0
        it "should return the size of the nubbed input." $ property $ \(ts :: [Text]) ->
            size (fromList ts) `shouldBe` uniqueCount ts

    describe "memberItem" $ do
        it "should return False for empty indexes." $ property $ \(t :: Text) ->
            not $ memberItem t I.empty
        it "should return whether the item is in the input." $ property $ \(t :: Text) (ts :: [Text]) ->
            memberItem t (fromList ts) == (t `elem` ts) && memberItem t (fromList (t:ts))

    describe "memberIx" $ do
        it "should return False for empty indexes." $ property $ \i ->
            not $ memberIx i I.empty
        it "should return whether the item is in the input." $ property $ \(NonNegative ix) (ts :: [Text]) ->
            memberIx ix (fromList ts) == (ix < uniqueCount ts)

    describe "insertItem" $ do
        it "should add an item to an empty index." $ property $ \(t :: Text) ->
            let i = insertItem t I.empty
            in  size i == 1 && toList i == [t]
        it "should add an item to an existing index." $ property $ \(t :: Text) (ts :: [Text]) ->
            let i      = insertItem t $ fromList ts
                offset = if t `elem` ts then 0 else 1
            in  size i == (offset + uniqueCount ts)

    describe "lookupItem" $ do
        it "should return the index of an item in the list." $ property $ \(ts :: [Text]) ->
            let i = fromList ts
            in  L.all (isJust . (`lookupItem` i)) ts
        it "should return nothing for items not in the list." $ property $ \(t :: Text) (ts :: [Text]) ->
            let p = if t `elem` ts then isJust else isNothing
            in  p . lookupItem t $ fromList ts

    describe "lookupIx" $ do
        it "should return the item for an index in the list." $ property $ \(NonEmpty (ts :: [Text])) ->
            let i = fromList ts
            in  L.all (isJust . (`lookupIx` i)) ([0 .. uniqueCount ts - 1] :: [Int])
        it "should return Nothing for an index not in the list." $ property $ \(ts :: [Text]) ->
            isNothing . lookupIx (1 + L.length ts) $ fromList ts

    describe "toList" $ do
        it "should return an empty list for an empty index." $
            toList (I.empty :: IxIndex Text) `shouldSatisfy` L.null
        it "should return a list of the same size as itself." $ property $ \(ts :: [Text]) ->
            let i = fromList ts
            in  L.length (toList i) == I.size i

    describe "fromList" $ do
        it "should return an empty index for an empty list." $
            fromList ([] :: [Text]) `shouldSatisfy` I.null
        it "should return an index the size of the number of unique entries in the input." $
            property $ \(ts :: [Text]) -> I.size (fromList ts) == uniqueCount ts
