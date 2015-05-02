{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output.KwicSpec where


import qualified Data.Text           as T

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Output.Kwic
import           StatNLP.Types


instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary LinePos where
    arbitrary = Line <$> arbitrary `suchThat` (>=0)
                     <*> arbitrary `suchThat` (>=0)
                     <*> arbitrary `suchThat` (>=0)


spec :: Spec
spec = do
    describe "syncLines" $ do
        it "should should return nothing with no input lines." $
            property $ \lines ->
                syncLines [] lines == []
        it "should return nothing with no hits." $
            property $ \hits ->
                syncLines hits [] == []
        it "should sync input lines with hit positions." $
            let inputs = zip [0..] ["one", "two", "three", "four", "five"]
                hits   = [ [Line 1 2 3, Line 1 4 5]
                         , [Line 3 4 5]
                         , [Line 4 5 6, Line 4 7 8, Line 4 9 10]
                         ]
            in  syncLines hits inputs
                    `shouldBe` [ (1, "two",  [Line 1 2 3, Line 1 4 5])
                               , (3, "four", [Line 3 4 5])
                               , (4, "five", [ Line 4 5 6
                                             , Line 4 7 8
                                             , Line 4 9 10
                                             ])]
