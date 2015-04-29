{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.TokensSpec where


import           Data.Char
import qualified Data.Text           as T
import           Data.Text.ICU       hiding (normalize)

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Text.Tokens
import           StatNLP.Types


instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

newtype Alphabetic = Alphabetic T.Text
                     deriving (Show)

instance Arbitrary Alphabetic where
    arbitrary =   Alphabetic
              .   T.pack
              <$> listOf
              (   elements " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")


spec :: Spec
spec = do
    let tokenize = posTokenizer (regex [UnicodeWord] "\\w+")

    describe "posTokenizer" $ do
        it "should yield monotonically increasing column numbers within a line." $
            property $ \input ->
                let step (i, prev) j = (j, prev && i < j)
                    tokens = tokenize input
                    cols   = map (spanStart . tokenPos) tokens
                    (_, increasing) = foldl step ((-1), True) cols
                in  increasing
        it "should tokenize a string based on the regex." $
            let input = "Now is the time for all good people to come to the\
                        \ aid of their country."
                token n i = Token n Nothing . Span i $ T.length n
            in  tokenize input `shouldBe` [ token "Now"     0
                                          , token "is"      4
                                          , token "the"     7
                                          , token "time"    11
                                          , token "for"     16
                                          , token "all"     20
                                          , token "good"    24
                                          , token "people"  29
                                          , token "to"      36
                                          , token "come"    39
                                          , token "to"      44
                                          , token "the"     47
                                          , token "aid"     51
                                          , token "of"      55
                                          , token "their"   58
                                          , token "country" 64
                                          ]

    describe "normalize" $ do
        it "should lower-case the tokens." $
            property $ \(Alphabetic input) ->
                all (\(Token n _ _) -> T.all isLower n)
                    . map normalize
                    $ tokenize input
        it "should lower-case the tokens." $
            let input = "Now is the time for all good people to come to the\
                        \ aid of their country."
            in  map (tokenNorm . normalize) (tokenize input)
                    `shouldBe` [ "now", "is", "the", "time", "for", "all"
                               , "good", "people", "to", "come", "to", "the"
                               , "aid", "of", "their", "country"
                               ]
