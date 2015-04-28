{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Tokens where


import           Data.Maybe
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Data.Text.ICU

import           StatNLP.Types


posTokenizer :: Regex -> Int -> Tokenizer (Token LinePos)
posTokenizer re line = mapMaybe (matchToken line) . findAll re

tokenize :: Int -> Tokenizer (Token LinePos)
tokenize line = posTokenizer (regex [UnicodeWord] "[\\p{L}\\p{M}]+") line

matchToken :: Int -> Match -> Maybe (Token LinePos)
matchToken line g = Token <$> text
                          <*> text
                          <*> pure Nothing
                          <*> (LinePos line . T.length <$> prefix 0 g)
    where text = group 0 g

normalize :: Token p -> Token p
normalize = omap T.toLower
