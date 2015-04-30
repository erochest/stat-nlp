{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module StatNLP.ContextSpec where


import           Control.Comonad
import           Data.Foldable   (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence   as Seq

import           Test.Hspec
import           Test.QuickCheck

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
    describe "Functor" $ do
        it "should satisfy fmap id == id." $
            property $ \(c :: Context Int) ->
                fmap id c == id c
        it "should satisfy fmap (f . g) == fmap f . fmap g." $
            property $ \(c :: Context Int) ->
                fmap ((* 2) . (+ 7)) c == (fmap (* 2) . fmap (+ 7)) c

    describe "pushRight" $ do
        it "should maintain the size of the left context." $
            property $ \(c :: Context Int) (xs :: [Int]) ->
                length (contextBefore $ foldl' (flip pushRight) c xs) == contextBeforeN c
        it "should maintain the size of the right context." $
            property $ \(c :: Context Int) (xs :: [Int]) ->
                length (contextAfter $ foldl' (flip pushRight) c xs) == contextAfterN c
        it "should shift everything over one." $
            (0 `pushRight` Context 2 2 [1, 2] 3 [4, 5]) `shouldBe` (Context 2 2 [0, 1] 2 [3, 4])

    describe "pushLeft" $ do
        it "should maintain the size of the left context." $
            property $ \(c :: Context Int) (xs :: [Int]) ->
                length (contextBefore $ foldl' pushLeft c xs) == contextBeforeN c
        it "should maintain the size of the right context." $
            property $ \(c :: Context Int) (xs :: [Int]) ->
                length (contextAfter $ foldl' pushLeft c xs) == contextAfterN c
        it "should shift everything over one." $
            (Context 2 2 [1, 2] 3 [4, 5] `pushLeft` 6) `shouldBe` (Context 2 2 [2, 3] 4 [5, 6])

    {-
     - describe "Comonad" $ do
     -     it "should satisfy extract . duplicate == id." $
     -         property $ \(c :: Context Int) ->
     -             (extract . duplicate) c == id c
     -     it "should satisfy extract . fmap f == f . extract." $
     -         property $ \(c :: Context Int) ->
     -             (extract . fmap (*7)) c == ((*7) . extract) c
     -     it "should satisfy fmap extract . duplicate == id." $
     -         property $ \(c :: Context Int) ->
     -             (fmap extract . duplicate) c == id c
     -     it "should satisfy duplicate . duplicate == fmap duplicate . duplicate" $
     -         property $ \(c :: Context Int) ->
     -             (fmap duplicate . duplicate) c == (duplicate . duplicate) c
     -     it "should satisfy duplicate = extend id." $
     -         property $ \(c :: Context Int) ->
     -             duplicate c == extend id c
     -     it "should satisfy fmap (fmap f) . duplicate = duplicate . fmap f." $
     -         property $ \(c :: Context Int) ->
     -             (fmap (fmap (*4)) . duplicate) c == (duplicate . fmap (*4)) c
     -     it "should satisfy extend f = fmap f . duplicate." $
     -         property $ \(c :: Context Int) ->
     -             extend sumc c == (fmap sumc . duplicate) c
     -}
