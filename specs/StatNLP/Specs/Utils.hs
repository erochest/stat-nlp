

module StatNLP.Specs.Utils where


import qualified Data.Text       as T

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
