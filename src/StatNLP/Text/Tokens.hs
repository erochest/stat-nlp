{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Tokens where


import           Data.Maybe
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Data.Text.ICU

import           StatNLP.Types


posTokenizer :: Regex -> Tokenizer (Token SpanPos)
posTokenizer re = mapMaybe matchToken . findAll re

tokenize :: Tokenizer (Token SpanPos)
tokenize = posTokenizer (regex [UnicodeWord] "[\\p{L}\\p{M}]+")

matchToken :: Match -> Maybe (Token SpanPos)
matchToken g = Token <$> text
                     <*> pure Nothing
                     <*> (Span <$> fmap T.length (prefix 0 g)
                               <*> fmap T.length text)
    where text = group 0 g

normalize :: Token p -> Token p
normalize = omap T.toLower
