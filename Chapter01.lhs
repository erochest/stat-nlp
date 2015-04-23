> {-# LANGUAGE OverloadedStrings #-}

> module Main where

> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO
> import Taygeta.Tokenizer (regexTokenizer)
> import Taygeta.Types

**TODO**: Research Wittgenstein's *use theory of meaning*.

TODO: tokenization
TODO: frequency
TODO: token/type ratio
TODO: collocate generator
TODO: n-gram generator
TODO: stopword filter
TODO: inverse index
TODO: concordance/kwic generator
TODO: kwic frequency tree


> tokenizer :: PlainTokenizer
> tokenizer = regexTokenizer "[\\p{L}\\p{M}]+"

> main :: IO ()
> main = TIO.interact (T.unlines . concatMap tokenizer . T.lines)

