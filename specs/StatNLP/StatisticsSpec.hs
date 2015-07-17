module StatNLP.StatisticsSpec where


import           Control.Monad
import           Data.Bifunctor
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Tuple          (swap)

import           Test.Hspec
import           Test.HUnit

import           StatNLP.Specs.Utils
import           StatNLP.Statistics
import           StatNLP.Types
import           StatNLP.Utils


spec :: Spec
spec = do
    describe "sgt" $ do
        it "should replicate the output of SGT.c." $ do
            input    <- readHashMap "./data/austen-cntcnt.txt" :: IO (M.HashMap Int Int)
            expected <- readHashMap "./data/austen-sgt.txt"    :: IO (M.HashMap Int Double)

            let actual = uncurry (M.insert 0)
                       . swap
                       $ sgt (MHash $ Sum <$> input) 1.96
                m      = 0.01
            M.keys actual `shouldMatchList` M.keys expected
            forM_ (M.keys actual) $ \k ->
                let a = M.lookupDefault 0 k actual
                    e = M.lookupDefault 0 k expected
                in  assertBool (  "Wrong on " ++ show k ++ ": "
                               ++ show a ++ " != " ++ show e)
                               $ abs (a - e) <= m
