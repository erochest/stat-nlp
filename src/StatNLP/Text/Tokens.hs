{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Tokens where


import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import           Data.Maybe
import           Data.MonoTraversable
import qualified Data.Text            as T
import           Data.Text.ICU
import           Data.Traversable

import           StatNLP.Types


posTokenizer :: Regex -> Tokenizer (Token SpanPos PlainToken)
posTokenizer re = mapMaybe matchToken . findAll re

lineTokenizer :: Regex -> Int -> Tokenizer (Token LinePos PlainToken)
lineTokenizer re offset input =
    concatMap (uncurry (map . linePos))
        . zipWith (curry (fmap (posTokenizer re))) [0..]
        $ T.lines input
    where
        linePos line t@Token{_tokenPos} =
            t & tokenPos .~ Line (offset + line)
                                 (_spanStart _tokenPos)
                                 (_spanEnd _tokenPos)

tokenize :: Tokenizer (Token LinePos PlainToken)
tokenize = lineTokenizer (regex [UnicodeWord] "[\\p{L}\\p{M}]+") 0

matchToken :: Match -> Maybe (Token SpanPos PlainToken)
matchToken g = do
    text  <- group 0 g
    start <- T.length <$> prefix 0 g
    return . Token text Nothing . Span start $ start + T.length text

normalize :: Token p PlainToken -> Token p PlainToken
normalize = fmap T.toLower

cacheTokens :: (Eq n, Hashable n, Traversable t)
            => Cache n -> t (Token a n) -> (Cache n, t (Token a n))
cacheTokens = mapAccumL cacheToken
    where
        cacheToken c t =
            let norm = _tokenNorm t
            in  case M.lookup norm c of
                    Nothing    -> (M.insert norm norm c, t)
                    Just norm' -> (c, t & tokenNorm .~ norm')
