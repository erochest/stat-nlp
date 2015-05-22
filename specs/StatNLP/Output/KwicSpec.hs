{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module StatNLP.Output.KwicSpec where


import           Control.Arrow
import           Control.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.List           as L
import           Data.Ord
import qualified Data.Text           as T

import           Test.Hspec
import           Test.QuickCheck

import           StatNLP.Document    (documentKey, readInverseIndexDocument)
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
        let docs     = [ ("<document-0>", Document "<document-0>" S.empty Nothing ())
                       , ("<document-1>", Document "<document-1>" S.empty Nothing ())
                       , ("<document-2>", Document "<document-2>" S.empty Nothing ())
                       , ("<document-3>", Document "<document-3>" S.empty Nothing ())
                       , ("<document-4>", Document "<document-4>" S.empty Nothing ())
                       ] :: M.HashMap T.Text (Document p ())
            doc5     = [ ("<document-5>", Document "<document-5>" S.empty Nothing ())
                       ] :: M.HashMap T.Text (Document p ())
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
                       , ("<document-5>", "Yesterday is gone;\n\
                                          \ 'tomorrow'\n\
                                          \ doesn't exist;\n\
                                          \ today is all there is.")
                       ] :: M.HashMap T.Text T.Text
            readDoc  = return . flip (M.lookupDefault "") contents . documentKey
            corpus   = Corpus docs tokenize readDoc
            corpus5  = Corpus doc5 tokenize readDoc

        index  <- runIO $ fold <$> mapM (readInverseIndexDocument corpus ) (M.elems docs)
        index5 <- runIO $ fold <$> mapM (readInverseIndexDocument corpus5) (M.elems doc5)
        let kwic10 = fmap (L.sortBy (comparing (_posLine . snd . _kwicPos))) . kwic 10 corpus  index
            kwic5  = fmap (L.sortBy (comparing (_posLine . snd . _kwicPos))) . kwic 10 corpus5 index5

        it "should properly format a single hit in a corpus." $
            kwic10 "m" `shouldReturn`
                [Kwic ("<document-0>", Line 0 24 25) "g h i j k l" "m" "n o p q r"]
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
                [ Kwic ("<document-4>", Line 2  4  7) "hh ii jj kk"  "ljl" "mm ljl oo"
                , Kwic ("<document-4>", Line 2 11 14) "jj kk ljl mm" "ljl" "oo pp qq rr"
                , Kwic ("<document-4>", Line 4  7 10) "ss tt uu vv"  "ljl" "xx yy zz"
                ]
        it "should identify KWICs from multiple documents." $
            kwic10 "qq" `shouldReturn`
                [ Kwic ("<document-1>", Line 3  4  6) "mkm nn oo pp" "qq" "rr ss tt uu"
                , Kwic ("<document-2>", Line 3 10 12) "mjm nn oo pp" "qq" "rr ss tt uu"
                , Kwic ("<document-3>", Line 3  4  6) "mm nn oo pp"  "qq" "hjh ss tt"
                , Kwic ("<document-4>", Line 3  4  6) "mm ljl oo pp" "qq" "rr ss tt uu"
                ]
        it "should include punctuation and non-token characters in the output." $
            kwic5 "today" `shouldReturn`
                [ Kwic ("<document-5>", Line 3 1 6) "doesn't exist;" "today" "is all there"
                ]
        it "should include punctuation and non-token characters immediately adjacent to the target." $
            kwic5 "tomorrow" `shouldReturn`
                [ Kwic ("<document-5>", Line 1 2 10) "Yesterday is gone; '" "tomorrow" "' doesn't exist"
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

    describe "sliceHits" $ do
        let updateLine n Token{_tokenPos} = _tokenPos & posLine +~ n
            tokenizeLines :: T.Text -> [(T.Text, LinePos)]
            tokenizeLines = concat
                          . fmap ( sequenceA
                                 . uncurry (fmap . fmap . updateLine)
                                 . fmap (id &&& tokenize))
                          . zip [0..]
                          . T.lines
        it "should handle empty token sequences." $
            sliceHits [] `shouldBe` []
        it "should slice from the middle of a single line." $
            let tokens = tokenizeLines "This is the first day of the rest of my life."
                input  = L.take 3 $ L.drop 2 tokens
            in  sliceHits input `shouldBe` ["the first day"]
        it "should slice spanning two lines." $
            let tokens = tokenizeLines "This is the first day\nof the rest of my life."
                input  = L.take 6 $ L.drop 2 tokens
            in  sliceHits input `shouldBe` ["the first day", "of the rest"]
        it "should slice including a whole inner line." $
            let tokens = tokenizeLines "This is the first day\nof the rest\nof my life."
                input  = L.take 6 $ L.drop 2 tokens
            in  sliceHits input `shouldBe` ["the first day", "of the rest"]
