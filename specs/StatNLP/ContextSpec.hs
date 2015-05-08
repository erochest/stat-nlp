{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.ContextSpec where


import           Control.Comonad
import qualified Data.FingerTree as FT
import           Data.Foldable   (foldl')
import qualified Data.List       as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence   as Seq

import           Test.Hspec
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
sumc (Context b a as c bs) = c + foldl' (+) 0 as + foldl' (+) 0 bs

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
                    length (contextBefore $ foldl' (flip pushR) c xs) == contextBeforeN c
            it "should maintain the size of the right context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (contextAfter $ foldl' (flip pushR) c xs) == contextAfterN c
            it "should shift everything over one." $
                (0 `pushR` Context 2 2 [1, 2] 3 [4, 5]) `shouldBe` (Context 2 2 [0, 1] 2 [3, 4])

        describe "pushL" $ do
            it "should maintain the size of the left context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (contextBefore $ foldl' pushL c xs) == contextBeforeN c
            it "should maintain the size of the right context." $
                property $ \(c :: Context Int) (xs :: [Int]) ->
                    length (contextAfter $ foldl' pushL c xs) == contextAfterN c
            it "should shift everything over one." $
                (Context 2 2 [1, 2] 3 [4, 5] `pushL` 6) `shouldBe` (Context 2 2 [2, 3] 4 [5, 6])

        describe "shiftL" $ do
            it "should shift to nothing on an empty context." $
                shiftL (Context 2 2 [1, 2] 3 []) `shouldSatisfy` isNothing
            it "should shift to something on an existing context." $
                shiftL (Context 2 2 [1, 2] 3 [4]) `shouldSatisfy` isJust
        describe "shiftR" $ do
            it "should shift to nothing on an empty context." $
                shiftR (Context 2 2 [] 1 [2, 3]) `shouldSatisfy` isNothing
            it "should shift to something on an existing context." $
                shiftR (Context 2 2 [1] 2 [3, 4]) `shouldSatisfy` isJust

    describe "MeasuredContext" $ do
        let goodbye = appendContext (MContext 10 FT.empty) ([ "good-bye"
                                                            , "cruel"
                                                            , "world"
                                                            ] :: [Token SpanPos])
            today   = appendContext (MContext 10 FT.empty) ([ "today"
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
                                                            ] :: [ContextItem (Token SpanPos)])

        describe "pushContext" $ do
            it "should allow one to push context into it." $
                getContext (pushContext (MContext 10 FT.empty) ("howdy" :: Token SpanPos))
                    `shouldBe` ["howdy"]
            it "should limit the amount of data in the context." $
                getContext goodbye `shouldBe` ["cruel", "world"]
            it "should allow one item of data over the limit." $
                FT.measure (mContextSeq goodbye) `shouldBe` Sum 12

        describe "appendContext" $ do
            it "should push multiple items into the context." $
                let c = appendContext (MContext 100 FT.empty)
                                      (["one", "two", "three"] :: [Token SpanPos])
                in  getContext c `shouldBe` ["one", "two", "three"]
            it "should push lots of things into the list." $
                getContext today `shouldBe` ["my", "life", "."]

        describe "getContext" $ do
            it "should return the context as a list." $
                getContext today `shouldBe` ["my", "life", "."]
