{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.TokensSpec where


import           Data.Char
import qualified Data.Text           as T
import           Data.Text.ICU       hiding (normalize)

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Specs.Utils
import           StatNLP.Text.Tokens
import           StatNLP.Types


spec :: Spec
spec = do
    let tokenize' = posTokenizer  (regex [UnicodeWord] "\\w+")
        ltokenize = lineTokenizer (regex [UnicodeWord] "\\w+") 0

    describe "posTokenizer" $ do
        it "should yield monotonically increasing column numbers within a line." $
            property $ \input ->
                let step (i, prev) j = (j, prev && i < j)
                    tokens = tokenize' input
                    cols   = map (_spanStart . _tokenPos) tokens
                    (_, increasing) = foldl step (-1, True) cols
                in  increasing
        it "should tokenize a string based on the regex." $
            let input = "Now is the time for all good people to come to the\
                        \ aid of their country."
                token n i = Token n Nothing . Span i $ i + T.length n
            in  tokenize' input `shouldBe` [ token "Now"     0
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

    describe "lineTokenizer" $ do
        it "should correctly identify the line numbers of the tokens." $
            let input = "Now is the time for\nall good people to come\nto the\
                        \ aid of their\ncountry."
                token n l i = Token n Nothing . Line l i $ i + T.length n
            in  ltokenize input `shouldBe` [ token "Now"     0 0
                                           , token "is"      0 4
                                           , token "the"     0 7
                                           , token "time"    0 11
                                           , token "for"     0 16
                                           , token "all"     1 0
                                           , token "good"    1 4
                                           , token "people"  1 9
                                           , token "to"      1 16
                                           , token "come"    1 19
                                           , token "to"      2 0
                                           , token "the"     2 3
                                           , token "aid"     2 7
                                           , token "of"      2 11
                                           , token "their"   2 14
                                           , token "country" 3 0
                                           ]
        it "should return end indexes, not lengths." $
            property $ \input ->
                all (\(Token _ _ (Line _ start end)) -> start < end)
                    (ltokenize input)

    describe "normalize" $ do
        it "should lower-case the tokens." $
            property $ \(Alphabetic input) ->
                all (\(Token n _ _) -> T.all isLower n)
                    . map normalize
                    $ tokenize input
        it "should lower-case the tokens." $
            let input = "Now is the time for all good people to come to the\
                        \ aid of their country."
            in  map (_tokenNorm . normalize) (tokenize input)
                    `shouldBe` [ "now", "is", "the", "time", "for", "all"
                               , "good", "people", "to", "come", "to", "the"
                               , "aid", "of", "their", "country"
                               ]
