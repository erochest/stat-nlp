

module StatNLP.Specs.Utils where


import qualified Data.Text       as T

import           Test.Hspec
import           Test.QuickCheck


instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

newtype Alphabetic = Alphabetic T.Text
                     deriving (Show)

instance Arbitrary Alphabetic where
    arbitrary =   Alphabetic
              .   T.pack
              <$> listOf
              (   elements " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

shouldApproximate :: Double -> Double -> Double -> Expectation
shouldApproximate z a b = abs (a - b) `shouldSatisfy` (< z)
