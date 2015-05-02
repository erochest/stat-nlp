{-# LANGUAGE NamedFieldPuns    #-}
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

lineTokenizer :: Regex -> Int -> Tokenizer (Token LinePos)
lineTokenizer re offset input =
    concatMap (uncurry (map . linePos))
        . zipWith (curry (fmap (posTokenizer re))) [0..]
        $ T.lines input
    where
        linePos line t@Token{tokenPos} =
            t { tokenPos = Line (offset + line)
                                (spanStart tokenPos)
                                (spanEnd tokenPos)
              }

tokenize :: Tokenizer (Token LinePos)
tokenize = lineTokenizer (regex [UnicodeWord] "[\\p{L}\\p{M}]+") 0

matchToken :: Match -> Maybe (Token SpanPos)
matchToken g = do
    text  <- group 0 g
    start <- T.length <$> prefix 0 g
    return . Token text Nothing . Span start $ start + T.length text

normalize :: Token p -> Token p
normalize = omap T.toLower

cacheTokens :: Traversable t
            => Cache PlainToken -> t (Token a) -> (Cache PlainToken, t (Token a))
cacheTokens c ts = mapAccumL cacheToken c ts
    where
        cacheToken c t =
            let norm = tokenNorm t
            in  case M.lookup norm c of
                    Nothing    -> (M.insert norm norm c, t)
                    Just norm' -> (c, t { tokenNorm = norm' })
