{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Tokens where


import qualified Data.HashMap.Strict  as M
import           Data.Maybe
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Data.Text.ICU
import           Data.Traversable

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

cacheTokens :: Cache PlainToken -> [Token a] -> (Cache PlainToken, [Token a])
cacheTokens c ts = mapAccumL cacheToken c ts
    where
        cacheToken c t =
            let norm = tokenNorm t
            in  case M.lookup norm c of
                    Nothing    -> (M.insert norm norm c, t)
                    Just norm' -> (c, t { tokenNorm = norm' })
