{-# LANGUAGE TupleSections #-}


module StatNLP.StatisticsSpec where


import           Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Monoid
import           Data.Tuple          (swap)

import           Test.Hspec
import           Test.HUnit

import           StatNLP.Statistics
import           StatNLP.Types
import           StatNLP.Utils


spec :: Spec
spec = describe "sgt" $
       it "should replicate the output of SGT.c." $ do
            input    <- readHashMap "./data/austen-cntcnt.txt" :: IO (M.HashMap Int Int)
            expected <- readHashMap "./data/austen-sgt.txt"    :: IO (M.HashMap Int Double)

            let actual = fmap snd
                       . uncurry (M.insert 0)
                       . swap
                       . fmap (0,)
                       $ sgt (MHash $ Sum <$> input) 1.96
                m      = 1.0e-4
            M.keys actual `shouldMatchList` M.keys expected
            forM_ (L.sort $ M.keys actual) $ \k ->
                let a = M.lookupDefault 0 k actual
                    e = M.lookupDefault 0 k expected
                in  assertBool (  "WRONG " ++ show k ++ ": "
                               ++ show a ++ " != " ++ show e
                               ++ " (" ++ show (abs (a - e)) ++ ")")
                               $ abs (a - e) <= m
