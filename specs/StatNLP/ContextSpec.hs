{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- # ANN module "HLint: ignore Functor law" #-}
{- # ANN module "HLint: ignore Evaluate" #-}
{- # ANN module "HLint: ignore Use ." #-}


module StatNLP.ContextSpec where


import qualified Data.FingerTree as FT
import           Data.Foldable   (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence   as Seq

import           Test.Hspec      hiding (after, before)
import           Test.QuickCheck

import           StatNLP.Context
import           StatNLP.Types


instance Arbitrary a => Arbitrary (Context a) where
    arbitrary = do
        before <- arbitrary `suchThat` (>=0)
        after  <- arbitrary `suchThat` (>=0)
        Context before after <$> fmap (Seq.fromList . take before) (infiniteListOf arbitrary)
                             <*> arbitrary
                             <*> fmap (Seq.fromList . take after)  (infiniteListOf arbitrary)


sumc :: Num a => Context a -> a
sumc (Context _ _ as c bs) = c + foldl' (+) 0 as + foldl' (+) 0 bs

spec :: Spec
spec = do
    describe "Context" $ do
        describe "Functor" $ do
            it "should satisfy fmap id == id." $
                property $ \(c :: Context Int) ->
                    fmap id c == id c
            it "should satisfy fmap (f . g) == fmap f . fmap g." $
                property $ \(c :: Context Int) ->
                    fmap ((* 2) . (+ 7)) c == (fmap (* 2) . fmap (+ 7)) c

        describe "pushR" $ do
            it "should maintain the size of the left context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (_contextBefore $ foldl' (flip pushR) c xs)
                               == _contextBeforeN c
            it "should maintain the size of the right context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (_contextAfter $ foldl' (flip pushR) c xs)
                               == _contextAfterN c
            it "should shift everything over one." $
                (0 `pushR` Context 2 2 [1 :: Int, 2] 3 [4, 5]) `shouldBe`
                    Context 2 2 [0, 1] 2 [3, 4]

        describe "pushL" $ do
            it "should maintain the size of the left context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (_contextBefore $ foldl' pushL c xs)
                               == _contextBeforeN c
            it "should maintain the size of the right context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (_contextAfter $ foldl' pushL c xs)
                               == _contextAfterN c
            it "should shift everything over one." $
                (Context 2 2 [1 :: Int, 2] 3 [4, 5] `pushL` 6) `shouldBe`
                    Context 2 2 [2, 3] 4 [5, 6]

        describe "shiftL" $ do
            it "should shift to nothing on an empty context." $
                shiftL (Context 2 2 [1 :: Int, 2] 3 []) `shouldSatisfy`
                       isNothing
            it "should shift to something on an existing context." $
                shiftL (Context 2 2 [1 :: Int, 2] 3 [4]) `shouldSatisfy` isJust
        describe "shiftR" $ do
            it "should shift to nothing on an empty context." $
                shiftR (Context 2 2 [] 1 [2 :: Int, 3]) `shouldSatisfy`
                       isNothing
            it "should shift to something on an existing context." $
                shiftR (Context 2 2 [1 :: Int] 2 [3, 4]) `shouldSatisfy`
                       isJust

    describe "MeasuredContext" $ do
        let goodbye = appendLeft (MContext 10 FT.empty) ([ "good-bye"
                                                         , "cruel"
                                                         , "world"
                                                         ] :: [Token SpanPos PlainToken])
            today   = appendLeft (MContext 10 FT.empty) ([ "today"
                                                         , "is"
                                                         , "the"
                                                         , "first"
                                                         , "day"
                                                         , "of"
                                                         , "the"
                                                         , ","
                                                         , "rest"
                                                         , "of"
                                                         , "my"
                                                         , "life"
                                                         , "."
                                                         ] :: [Token SpanPos PlainToken])

        describe "pushLeft" $ do
            it "should allow one to push context into it." $
                getContext (pushLeft ("howdy" :: Token SpanPos PlainToken) (MContext 10 FT.empty))
                    `shouldBe` ["howdy"]
            it "should limit the amount of data in the context." $
                getContext goodbye `shouldBe` ["cruel", "world"]
            it "should allow one item of data over the limit." $
                FT.measure (_mContextSeq goodbye) `shouldBe` Sum 12

        describe "appendLeft" $ do
            it "should push multiple items into the context." $
                let c = appendLeft (MContext 100 FT.empty)
                                      (["one", "two", "three"] :: [Token SpanPos PlainToken])
                in  getContext c `shouldBe` ["one", "two", "three"]
            it "should push lots of things into the list." $
                getContext today `shouldBe` ["of", "my", "life", "."]

        describe "getContext" $
            it "should return the context as a list." $
                getContext today `shouldBe` ["of", "my", "life", "."]
