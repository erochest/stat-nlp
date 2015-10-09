{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.NGramsSpec where


import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Ord
import qualified Data.Text           as T

import           Test.Hspec

import           StatNLP.Corpus
import           StatNLP.Document
import           StatNLP.Text.Freqs
import           StatNLP.Text.Tokens
import           StatNLP.Text.Utils
import           StatNLP.Types
import           StatNLP.Utils


spec :: Spec
spec = do
    let corpus = makeCorpus tokenizer
                 (return . getParagraph . T.pack . _documentId)
                 (map initDocument ["<paragraph0>", "<paragraph1>"])
    docs <- runIO $ readCorpusVectors Nothing corpus
    let tokens' = fmap (fmap _tokenNorm . _documentTokens)
                . L.sortBy (comparing _documentId)
                $ M.elems docs
        freqs'  = foldParMap frequencies tokens'
        hapax   = hapaxLegomena freqs'
        unknown = "*"
        tokens  = fmap (replaceFromSet hapax unknown) tokens'
        freqs   = replaceFreqsFromSet hapax unknown freqs'
        ngrms   = foldParMap (frequencies . trigramsV) tokens

    describe "tokenizer" $
        it "should read the right tokens." $
            tokens' `shouldBe` [ [ "before", "you", "use", "or", "read", "this"
                                 , "etext", "by", "using", "or", "reading", "any"
                                 , "part", "of", "this", "project", "gutenberg"
                                 , "tm", "etext", "you", "indicate", "that", "you"
                                 , "understand", "agree", "to", "and", "accept"
                                 , "this", "small", "print", "statement", "if"
                                 , "you", "do", "not", "you", "can", "receive"
                                 , "a", "refund", "of", "the", "money", "if"
                                 , "any", "you", "paid", "for", "this", "etext"
                                 , "by", "sending", "a", "request", "within"
                                 , "days", "of", "receiving", "it", "to", "the"
                                 , "person", "you", "got", "it", "from", "if"
                                 , "you", "received", "this", "etext", "on", "a"
                                 , "physical", "medium", "such", "as", "a", "disk"
                                 , "you", "must", "return", "it", "with", "your"
                                 , "request"
                                 ]
                               , [ "about", "project", "gutenberg"
                                 , "tm", "etexts", "this", "project", "gutenberg"
                                 , "tm", "etext", "like", "most", "project"
                                 , "gutenberg", "tm", "etexts", "is", "a"
                                 , "public", "domain", "work", "distributed"
                                 , "by", "professor", "michael", "s", "hart"
                                 , "through", "the", "project", "gutenberg"
                                 , "association", "at", "illinois", "benedictine"
                                 , "college", "the", "project", "among", "other"
                                 , "things", "this", "means", "that", "no", "one"
                                 , "owns", "a", "united", "states", "copyright"
                                 , "on", "or", "for", "this", "work", "so", "the"
                                 , "project", "and", "you", "can", "copy", "and"
                                 , "distribute", "it", "in", "the", "united"
                                 , "states", "without", "permission", "and"
                                 , "without", "paying", "copyright", "royalties"
                                 , "special", "rules", "set", "forth", "below"
                                 , "apply", "if", "you", "wish", "to", "copy"
                                 , "and", "distribute", "this", "etext", "under"
                                 , "the", "project", "s", "project", "gutenberg"
                                 , "trademark"
                                 ] ]

    describe "frequencies" $
        it "should calculate the frequencies correctly." $
            unHash freqs' `shouldBe`
                [ ("a", 6), ("about", 1), ("accept", 1), ("agree", 1)
                , ("among", 1), ("and", 5), ("any", 2), ("apply", 1)
                , ("as", 1), ("association", 1), ("at", 1)
                , ("before", 1), ("below", 1), ("benedictine", 1)
                , ("by", 3), ("can", 2), ("college", 1), ("copy", 2)
                , ("copyright", 2), ("days", 1), ("disk", 1)
                , ("distribute", 2), ("distributed", 1), ("do", 1)
                , ("domain", 1), ("etext", 6), ("etexts", 2)
                , ("for", 2), ("forth", 1), ("from", 1), ("got", 1)
                , ("gutenberg", 6), ("hart", 1), ("if", 4)
                , ("illinois", 1), ("in", 1), ("indicate", 1)
                , ("is", 1), ("it", 4), ("like", 1), ("means", 1)
                , ("medium", 1), ("michael", 1), ("money", 1)
                , ("most", 1), ("must", 1), ("no", 1), ("not", 1)
                , ("of", 3), ("on", 2), ("one", 1), ("or", 3)
                , ("other", 1), ("owns", 1), ("paid", 1), ("part", 1)
                , ("paying", 1), ("permission", 1), ("person", 1)
                , ("physical", 1), ("print", 1), ("professor", 1)
                , ("project", 9), ("public", 1), ("read", 1)
                , ("reading", 1), ("receive", 1), ("received", 1)
                , ("receiving", 1), ("refund", 1), ("request", 2)
                , ("return", 1), ("royalties", 1), ("rules", 1)
                , ("s", 2), ("sending", 1), ("set", 1), ("small", 1)
                , ("so", 1), ("special", 1), ("statement", 1)
                , ("states", 2), ("such", 1), ("that", 2), ("the", 7)
                , ("things", 1), ("this", 9), ("through", 1), ("tm", 4)
                , ("to", 3), ("trademark", 1), ("under", 1)
                , ("understand", 1), ("united", 2), ("use", 1)
                , ("using", 1), ("wish", 1), ("with", 1), ("within", 1)
                , ("without", 2), ("work", 2), ("you", 11), ("your", 1)
                ]

    describe "hapaxLegomena" $
        it "should find all tokens that occur once." $
            hapax `shouldBe` [ "about", "accept", "agree", "among", "apply"
                             , "as", "association", "at", "before", "below"
                             , "benedictine", "college", "days", "disk"
                             , "distributed", "do", "domain", "forth", "from"
                             , "got", "hart", "illinois", "in", "indicate", "is"
                             , "like", "means", "medium", "michael", "money"
                             , "most", "must", "no", "not", "one", "other"
                             , "owns", "paid", "part", "paying", "permission"
                             , "person", "physical", "print", "professor"
                             , "public", "read", "reading", "receive", "received"
                             , "receiving", "refund", "return", "royalties"
                             , "rules", "sending", "set", "small", "so", "special"
                             , "statement", "such", "things", "through"
                             , "trademark", "under", "understand", "use", "using"
                             , "wish", "with", "within", "your"
                             ]

    describe "replaceFromSet" $
        it "should replace any tokens that only occur once." $
            tokens `shouldBe` [ [ "*", "you", "*", "or", "*", "this"
                                , "etext", "by", "*", "or", "*", "any"
                                , "*", "of", "this", "project", "gutenberg"
                                , "tm", "etext", "you", "*", "that", "you"
                                , "*", "*", "to", "and", "*"
                                , "this", "*", "*", "*", "if"
                                , "you", "*", "*", "you", "can", "*"
                                , "a", "*", "of", "the", "*", "if"
                                , "any", "you", "*", "for", "this", "etext"
                                , "by", "*", "a", "request", "*"
                                , "*", "of", "*", "it", "to", "the"
                                , "*", "you", "*", "it", "*", "if"
                                , "you", "*", "this", "etext", "on", "a"
                                , "*", "*", "*", "*", "a", "*"
                                , "you", "*", "*", "it", "*", "*"
                                , "request"
                                ]
                              , [ "*", "project", "gutenberg"
                                , "tm", "etexts", "this", "project", "gutenberg"
                                , "tm", "etext", "*", "*", "project"
                                , "gutenberg", "tm", "etexts", "*", "a"
                                , "*", "*", "work", "*"
                                , "by", "*", "*", "s", "*"
                                , "*", "the", "project", "gutenberg"
                                , "*", "*", "*", "*"
                                , "*", "the", "project", "*", "*"
                                , "*", "this", "*", "that", "*", "*"
                                , "*", "a", "united", "states", "copyright"
                                , "on", "or", "for", "this", "work", "*", "the"
                                , "project", "and", "you", "can", "copy", "and"
                                , "distribute", "it", "*", "the", "united"
                                , "states", "without", "*", "and"
                                , "without", "*", "copyright", "*"
                                , "*", "*", "*", "*", "*"
                                , "*", "if", "you", "*", "to", "copy"
                                , "and", "distribute", "this", "etext", "*"
                                , "the", "project", "s", "project", "gutenberg"
                                , "*"
                                ] ]

    describe "replaceFreqsFromset" $
        it "should replace any tokens that only occur once." $
            unHash freqs `shouldBe`
                [ ("a", 6)
                , ("and", 5), ("any", 2)
                , ("by", 3), ("can", 2), ("copy", 2)
                , ("copyright", 2)
                , ("distribute", 2)
                , ("etext", 6), ("etexts", 2)
                , ("for", 2)
                , ("gutenberg", 6), ("if", 4)
                , ("it", 4)
                , ("of", 3), ("on", 2), ("or", 3)
                , ("project", 9)
                , ("request", 2)
                , ("s", 2)
                , ("states", 2), ("that", 2), ("the", 7)
                , ("this", 9), ("tm", 4)
                , ("to", 3)
                , ("united", 2)
                , ("without", 2), ("work", 2), ("you", 11)
                , ("*", 73)
                ]

    describe "trigramsV" $
        it "should identify the trigrams in the input." $
            unHash ngrms `shouldBe`
                  [ (("*", "*", "*"), 13)
                  , (("*", "*", "a"), 2)
                  , (("*", "*", "if"), 2)
                  , (("*", "*", "it"), 1)
                  , (("*", "*", "of"), 1)
                  , (("*", "*", "project"), 1)
                  , (("*", "*", "s"), 1)
                  , (("*", "*", "the"), 2)
                  , (("*", "*", "this"), 1)
                  , (("*", "*", "to"), 1)
                  , (("*", "*", "work"), 1)
                  , (("*", "*", "you"), 1)
                  , (("*", "a", "*"), 3)
                  , (("*", "a", "request"), 1)
                  , (("*", "a", "united"), 1)
                  , (("*", "and", "without"), 1)
                  , (("*", "any", "*"), 1)
                  , (("*", "by", "*"), 1)
                  , (("*", "copyright", "*"), 1)
                  , (("*", "for", "this"), 1)
                  , (("*", "if", "any"), 1)
                  , (("*", "if", "you"), 3)
                  , (("*", "it", "*"), 2)
                  , (("*", "it", "to"), 1)
                  , (("*", "of", "*"), 1)
                  , (("*", "of", "the"), 1)
                  , (("*", "of", "this"), 1)
                  , (("*", "or", "*"), 2)
                  , (("*", "project", "gutenberg"), 2)
                  , (("*", "s", "*"), 1)
                  , (("*", "that", "*"), 1)
                  , (("*", "that", "you"), 1)
                  , (("*", "the", "project"), 4)
                  , (("*", "the", "united"), 1)
                  , (("*", "this", "*"), 2)
                  , (("*", "this", "etext"), 2)
                  , (("*", "to", "and"), 1)
                  , (("*", "to", "copy"), 1)
                  , (("*", "work", "*"), 1)
                  , (("*", "you", "*"), 3)
                  , (("*", "you", "can"), 1)
                  , (("a", "*", "*"), 2)
                  , (("a", "*", "of"), 1)
                  , (("a", "*", "you"), 1)
                  , (("a", "request", "*"), 1)
                  , (("a", "united", "states"), 1)
                  , (("and", "*", "this"), 1)
                  , (("and", "distribute", "it"), 1)
                  , (("and", "distribute", "this"), 1)
                  , (("and", "without", "*"), 1)
                  , (("and", "you", "can"), 1)
                  , (("any", "*", "of"), 1)
                  , (("any", "you", "*"), 1)
                  , (("by", "*", "*"), 1)
                  , (("by", "*", "a"), 1)
                  , (("by", "*", "or"), 1)
                  , (("can", "*", "a"), 1)
                  , (("can", "copy", "and"), 1)
                  , (("copy", "and", "distribute"), 2)
                  , (("copyright", "*", "*"), 1)
                  , (("copyright", "on", "or"), 1)
                  , (("distribute", "it", "*"), 1)
                  , (("distribute", "this", "etext"), 1)
                  , (("etext", "*", "*"), 1)
                  , (("etext", "*", "the"), 1)
                  , (("etext", "by", "*"), 2)
                  , (("etext", "on", "a"), 1)
                  , (("etext", "you", "*"), 1)
                  , (("etexts", "*", "a"), 1)
                  , (("etexts", "this", "project"), 1)
                  , (("for", "this", "etext"), 1)
                  , (("for", "this", "work"), 1)
                  , (("gutenberg", "*", "*"), 1)
                  , (("gutenberg", "tm", "etext"), 2)
                  , (("gutenberg", "tm", "etexts"), 2)
                  , (("if", "any", "you"), 1)
                  , (("if", "you", "*"), 3)
                  , (("it", "*", "*"), 1)
                  , (("it", "*", "if"), 1)
                  , (("it", "*", "the"), 1)
                  , (("it", "to", "the"), 1)
                  , (("of", "*", "it"), 1)
                  , (("of", "the", "*"), 1)
                  , (("of", "this", "project"), 1)
                  , (("on", "a", "*"), 1)
                  , (("on", "or", "for"), 1)
                  , (("or", "*", "any"), 1)
                  , (("or", "*", "this"), 1)
                  , (("or", "for", "this"), 1)
                  , (("project", "*", "*"), 1)
                  , (("project", "and", "you"), 1)
                  , (("project", "gutenberg", "*"), 1)
                  , (("project", "gutenberg", "tm"), 4)
                  , (("project", "s", "project"), 1)
                  , (("request", "*", "*"), 1)
                  , (("s", "*", "*"), 1)
                  , (("s", "project", "gutenberg"), 1)
                  , (("states", "copyright", "on"), 1)
                  , (("states", "without", "*"), 1)
                  , (("that", "*", "*"), 1)
                  , (("that", "you", "*"), 1)
                  , (("the", "*", "if"), 1)
                  , (("the", "*", "you"), 1)
                  , (("the", "project", "*"), 1)
                  , (("the", "project", "and"), 1)
                  , (("the", "project", "gutenberg"), 1)
                  , (("the", "project", "s"), 1)
                  , (("the", "united", "states"), 1)
                  , (("this", "*", "*"), 1)
                  , (("this", "*", "that"), 1)
                  , (("this", "etext", "*"), 1)
                  , (("this", "etext", "by"), 2)
                  , (("this", "etext", "on"), 1)
                  , (("this", "project", "gutenberg"), 2)
                  , (("this", "work", "*"), 1)
                  , (("tm", "etext", "*"), 1)
                  , (("tm", "etext", "you"), 1)
                  , (("tm", "etexts", "*"), 1)
                  , (("tm", "etexts", "this"), 1)
                  , (("to", "and", "*"), 1)
                  , (("to", "copy", "and"), 1)
                  , (("to", "the", "*"), 1)
                  , (("united", "states", "copyright"), 1)
                  , (("united", "states", "without"), 1)
                  , (("without", "*", "and"), 1)
                  , (("without", "*", "copyright"), 1)
                  , (("work", "*", "by"), 1)
                  , (("work", "*", "the"), 1)
                  , (("you", "*", "*"), 3)
                  , (("you", "*", "for"), 1)
                  , (("you", "*", "it"), 1)
                  , (("you", "*", "or"), 1)
                  , (("you", "*", "that"), 1)
                  , (("you", "*", "this"), 1)
                  , (("you", "*", "to"), 1)
                  , (("you", "can", "*"), 1)
                  , (("you", "can", "copy"), 1)
                  ]


paragraphs :: [T.Text]
paragraphs = [ "\
    \ *BEFORE!* YOU USE OR READ THIS ETEXT\n\
    \ By using or reading any part of this PROJECT GUTENBERG-tm\n\
    \ etext, you indicate that you understand, agree to and accept\n\
    \ this \"Small Print!\" statement.  If you do not, you can receive\n\
    \ a refund of the money (if any) you paid for this etext by\n\
    \ sending a request within 30 days of receiving it to the person\n\
    \ you got it from.  If you received this etext on a physical\n\
    \ medium (such as a disk), you must return it with your request.\n"
    , "\
    \ ABOUT PROJECT GUTENBERG-TM ETEXTS\n\
    \ This PROJECT GUTENBERG-tm etext, like most PROJECT GUTENBERG-\n\
    \ tm etexts, is a \"public domain\" work distributed by Professor\n\
    \ Michael S. Hart through the Project Gutenberg Association at\n\
    \ Illinois Benedictine College (the \"Project\").  Among other\n\
    \ things, this means that no one owns a United States copyright\n\
    \ on or for this work, so the Project (and you!) can copy and\n\
    \ distribute it in the United States without permission and\n\
    \ without paying copyright royalties.  Special rules, set forth\n\
    \ below, apply if you wish to copy and distribute this etext\n\
    \ under the Project's \"PROJECT GUTENBERG\" trademark.\n\
    \ "
    ]

getParagraph :: T.Text -> T.Text
getParagraph "<paragraph0>" = head paragraphs
getParagraph "<paragraph1>" = paragraphs !! 1
getParagraph _              = T.empty
