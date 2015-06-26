{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module StatNLP.Text.Tokens where


import           Conduit
import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Text.ICU       hiding (normalize)
import           Data.Traversable

import           StatNLP.Document
import           StatNLP.Types


defTokenRE :: Regex
defTokenRE = regex [UnicodeWord] "[\\p{L}\\p{M}]+"

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
tokenize = lineTokenizer defTokenRE 0

tokenizeC :: Monad m => Conduit T.Text m (Token LinePos PlainToken)
tokenizeC = go 0
    where
        go n = do
            mline <- await
            case mline of
                Just line -> mapM_ yield (lineTokenizer defTokenRE n line)
                          >> go (succ n)
                Nothing   -> return ()

tokenNormC :: Monad m => Conduit T.Text m (Token LinePos PlainToken)
tokenNormC = tokenizeC =$= mapC normalize

tokenStopC :: Monad m
           => StopWords
           -> Conduit T.Text m (Token LinePos PlainToken)
tokenStopC stopwords =
    tokenNormC =$= filterC (not . (`S.member` stopwords) . _tokenNorm)

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

tokenizer :: Tokenizer LineToken
tokenizer = fmap normalize . tokenize

tokenizerStop :: StopWords -> Tokenizer LineToken
tokenizerStop stopwords = filter (not . (`S.member` stopwords) . _tokenNorm)
                        . tokenizer

filterToken :: PlainToken -> Document LineToken ts -> Bool
filterToken norm =
    documentContains (Token norm Nothing . Line 0 0 $ T.length norm)

posTokenIndex :: [Token p t] -> [Token Int t]
posTokenIndex = zipWith (set tokenPos) [0..]
