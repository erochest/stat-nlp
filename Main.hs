{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Text         as T
import qualified Data.Text.Format  as F
import           Taygeta.Tokenizer (regexTokenizer)

import           StatNLP.Output
import           StatNLP.Types
import           StatNLP.Utils

{-
 - **TODO**: Research Wittgenstein's *use theory of meaning*.
 -
 - TODO: collocate generator
 - TODO: n-gram generator
 - TODO: stopword filter
 - TODO: inverse index
 - TODO: concordance/kwic generator
 - TODO: kwic frequency tree
-}

main :: IO ()
main = do
    freqs <- runResourceT $  stdinC
                          $= CT.linesBounded (2^30)
                          $= concatMapC (regexTokenizer "[\\p{L}\\p{M}]+")
                          $= mapC T.toLower
                          $$ frequenciesC
    freqReport 25 freqs
    F.print "Token/type ratio = {}\n" . F.Only $ tokenTypeRatio freqs
