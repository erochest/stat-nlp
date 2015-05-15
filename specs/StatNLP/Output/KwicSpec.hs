{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Output.KwicSpec where


import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.Ord
import qualified Data.Text           as T

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Document    (documentKey)
import           StatNLP.Output.Kwic
import           StatNLP.Text.Index
import           StatNLP.Text.Tokens (tokenize)
import           StatNLP.Types


pending' :: Expectation -> Expectation
pending' m = pending

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary LinePos where
    arbitrary = Line <$> arbitrary `suchThat` (>=0)
                     <*> arbitrary `suchThat` (>=0)
                     <*> arbitrary `suchThat` (>=0)


spec :: Spec
spec = do
    describe "kwic" $ do
        let docs     = [ ("<document-0>", Document "<document-0>" S.empty)
                       , ("<document-1>", Document "<document-1>" S.empty)
                       , ("<document-2>", Document "<document-2>" S.empty)
                       , ("<document-3>", Document "<document-3>" S.empty)
                       , ("<document-4>", Document "<document-4>" S.empty)
                       ] :: M.HashMap T.Text Document
            contents = [ ("<document-0>", "a b c d e f g h i j k l m n o p q r s t u v w x y z")
                       , ("<document-1>", "aa bb cc dd ee\n\
                                          \ ff gg hh ii jj\n\
                                          \ kk ll mkm nn oo\n\
                                          \ pp qq rr ss tt\n\
                                          \ uu vv ww xx yy\n\
                                          \ zz\
                                          \")
                       , ("<document-2>", "aa bb cc dd ee\n\
                                          \ ff gg hh ii    jj\n\
                                          \ kk    ll    mjm    nn    oo\n\
                                          \    pp    qq rr ss tt\n\
                                          \ uu vv ww xx yy\n\
                                          \ zz\
                                          \")
                       , ("<document-3>", "aa bb cc dd ee\n\
                                          \ ff gg hjh ii jj\n\
                                          \ kk ll mm nn oo\n\
                                          \ pp qq hjh ss tt\n\
                                          \ uu vv ww xx yy\n\
                                          \ zz\
                                          \")
                       , ("<document-4>", "aa bb cc dd ee\n\
                                          \ ff gg hh ii jj\n\
                                          \ kk ljl mm ljl oo\n\
                                          \ pp qq rr ss tt\n\
                                          \ uu vv ljl xx yy\n\
                                          \ zz\
                                          \")
                       ] :: M.HashMap T.Text T.Text
            corpus   = Corpus docs tokenize
                              (return . flip (M.lookupDefault "") contents . documentKey)

        index <- runIO $ fold <$> mapM (readIndexDocument corpus) (M.elems docs)
        let kwic10 = fmap (L.sortBy (comparing (posLine . snd . kwicPos))) . kwic 10 corpus index

        it "should properly format a single hit in a corpus." $
            kwic10 "m" `shouldReturn`
                [Kwic ("<document-0>", Line 0 24 25) "g h i j k l" "m" "n o p q r s"]
        it "should wrap from the contexts from surrounding lines." $
            kwic10 "mkm" `shouldReturn`
                [Kwic ("<document-1>", Line 2 7 10) "ii jj kk ll" "mkm" "nn oo pp qq"]
        it "should normalize whitespace in contexts." $
            kwic10 "mjm" `shouldReturn`
                [Kwic ("<document-2>", Line 2 13 16) "ii jj kk ll" "mjm" "nn oo pp qq"]
        it "should identify non-overlapping hits." $
            kwic10 "hjh" `shouldReturn`
                [ Kwic ("<document-3>", Line 1 7 10) "dd ee ff gg" "hjh" "ii jj kk ll"
                , Kwic ("<document-3>", Line 3 7 10) "nn oo pp qq" "hjh" "ss tt uu vv"
                ]
        it "should identify overlapping hits." $
            kwic10 "ljl" `shouldReturn`
                [ Kwic ("<document-4>", Line 2  4  7) "hh ii jj kk"  "ljl" "mm ljl oo pp"
                , Kwic ("<document-4>", Line 2 11 14) "jj kk ljl mm" "ljl" "oo pp qq rr"
                , Kwic ("<document-4>", Line 4  7 10) "ss tt uu vv"  "ljl" "xx yy zz"
                ]
        it "should identify KWICs from multiple documents." $
            kwic10 "qq" `shouldReturn`
                [ Kwic ("<document-1>", Line 3  4  6) "mkm nn oo pp" "qq" "rr ss tt uu"
                , Kwic ("<document-2>", Line 3 10 12) "mjm nn oo pp" "qq" "rr ss tt uu"
                , Kwic ("<document-3>", Line 3  4  6) "mm nn oo pp"  "qq" "hjh ss tt uu"
                , Kwic ("<document-4>", Line 3  4  6) "mm ljl oo pp" "qq" "rr ss tt uu"
                ]

    describe "syncLines" $ do
        it "should should return nothing with no input lines." $
            property $ \hits ->
                syncLines hits [] == ([] :: [(Int, T.Text, [LinePos])])
        it "should return the input with no hits." $
            property $ \lines ->
                syncLines [] lines == ([(n, l, [])|(n, l) <- lines] :: [(Int, T.Text, [LinePos])])
        it "should sync input lines with hit positions." $
            let inputs = zip [0..] ["one", "two", "three", "four", "five"]
                hits   = [ [Line 1 2 3, Line 1 4 5]
                         , [Line 3 4 5]
                         , [Line 4 5 6, Line 4 7 8, Line 4 9 10]
                         ]
            in  syncLines hits inputs
                    `shouldBe` [ (0, "one",   [])
                               , (1, "two",   [Line 1 2 3, Line 1 4 5])
                               , (2, "three", [])
                               , (3, "four",  [Line 3 4 5])
                               , (4, "five",  [ Line 4 5 6
                                              , Line 4 7 8
                                              , Line 4 9 10
                                              ])]
